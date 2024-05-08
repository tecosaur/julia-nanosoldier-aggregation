using DataToolkit, DataFrames, Dates
using GLMakie, Colors, Loess

const ns_data = d"nanosoldier-collation"

ns_data.category = map(Symbol, ns_data.category)
ns_data.subcategory_1 = map(passmissing(Symbol), ns_data.subcategory_1)
ns_data.subcategory_2 = let symbpool = Dict{String, Symbol}()
    map(passmissing(c ->
        get!(() -> Symbol(
            if startswith(c, '(')
                try join(Meta.parse(c).args, ", ") catch _ c end
            else c end),
            symbpool, c)),
        ns_data.subcategory_2)
end

time_fmt(ns::Number) = if ns < 0 ""
elseif ns < 1000
    string(round(Int, ns), "ns")
elseif ns < 1000^2
    string(round(Int, ns ÷ 1000), "μs")
elseif ns < 1000^3
    string(round(Int, ns ÷ 1000^2), "ms")
else
    string(round(Int, ns ÷ 1000^3), "s")
end

time_fmt(nss::Vector{<:Number}) = map(time_fmt, nss)

pct_fmt(ps::Vector{<:Number}) =
    Makie.get_ticklabels(Makie.automatic, 100 .* ps) .* "%"

mem_fmt(m::Number) = if m < 0 ""
elseif m < 1000
    string(round(Int, m), "B")
elseif m < 1000^2
    string(round(Int, m ÷ 1000), "kB")
elseif m < 1000^3
    string(round(Int, m ÷ 1000^2), "MB")
else
    string(round(Int, m ÷ 1000^3), "GB")
end

mem_fmt(nss::Vector) = map(mem_fmt, nss)

date_fmt(t::Number) = string(Date(unix2datetime(t)))
date_fmt(ts::Vector{<:Number}) = map(date_fmt, ts)

function lchshift(c::C, Δl::Number=0, Δc::Number=0, Δh::Number=0) where {C <: Colorant}
    lc = convert(LCHab, c)
    convert(C, LCHab(clamp(lc.l + Δl, Float32(0), Float32(100)),
                     clamp(lc.c + Δc, Float32(0), Float32(150)),
                     (lc.h + Δh) % 360))
end

const colours = let base = (:time => colorant"#1c71d8",
                            :gctime => colorant"#f66151",
                            :mem => colorant"#26a269",
                            :alloc => colorant"#e5a50a")
    NamedTuple(map(base) do (name, bc)
                   name => (; base = bc,
                            dark = lchshift(bc, -15, -5),
                            light = lchshift(bc, 20, -10),
                            faint = lchshift(bc, 30, -40))
               end)
end

update_theme!(fonts = Attributes(
    bold = "Alegreya Sans Bold",
    bold_italic = "Alegreya Sans Bold Italic",
    italic = "Alegreya Sans Italic",
    regular = "Alegreya Sans Medium"))

function nanofig(default_category::String = String(first(ns_data.category)),
                 default_subcategory::String = "All",
                 default_benchmark::String = "All";
                 scatter::Bool = true, lines::Bool = true, smoothed::Bool=true)
    fig = Figure(size = (1400, 900))

    cat_menu = Menu(
        fig, options = sort(unique(ns_data.category)),
        default = default_category)

    subcategories = map(cat_menu.selection) do cat
        vals = [:All]
        append!(vals, unique(ns_data.subcategory_1[ns_data.category .=== cat]))
        sort(vals)
    end

    subcat_menu = Menu(fig, options = subcategories, default = default_subcategory, prompt="All")
    on(subcategories) do _; subcat_menu.selection[] = :All end

    benchmarks = map(subcat_menu.selection) do subcat
        vals = [:All]
        mask = ns_data.category .=== cat_menu.selection[]
        if subcat !== :All
            smask = ns_data.subcategory_1 .=== subcat
            mask = mask .& smask
        end
        append!(vals, skipmissing(unique(ns_data.subcategory_2[mask])))
        vals
    end

    bench_menu = Menu(fig, options = benchmarks, default = default_benchmark, prompt="All")
    on(benchmarks) do _; bench_menu.selection[] = :All end

    fig[3,1] = Label(fig, "Benchmark:", fontsize=18, font=:bold, justification=:right)
    fig[3,2] = cat_menu
    fig[3,3] = Label(fig, "/", fontsize=24)
    fig[3,4] = subcat_menu
    fig[3,5] = Label(fig, "/", fontsize=24)
    fig[3,6] = bench_menu
    fig[3,7] = Label(fig, "Scatter:", font=:bold, justification=:right)
    fig[3,8] = togl_scatter = Toggle(fig, active=scatter)
    fig[3,9] = Label(fig, "Lines:", font=:bold, justification=:right)
    fig[3,10] = togl_lines = Toggle(fig, active=lines)
    fig[3,11] = Label(fig, "Smoothed:", font=:bold, justification=:right)
    fig[3,12] = togl_smooth = Toggle(fig, active=smoothed)

    records_mask = map(cat_menu.selection, subcat_menu.selection, bench_menu.selection) do cat, subcat, bench
        mask = ns_data.category .=== cat
        if subcat !== :All
            mask = mask .& (ns_data.subcategory_1 .=== subcat)
        end
        if bench !== :All
            mask = mask .& (ns_data.subcategory_2 .=== bench)
        end
        mask
    end

    bench_tuples = collect(zip(ns_data.category, ns_data.subcategory_1, ns_data.subcategory_2))

    records_selection = map(records_mask) do mask
        btups = bench_tuples[mask]
        segments = indexin(btups, unique(btups))
        if issorted(segments)
            findall(mask)
        else
            findall(mask)[sortperm(segments)]
        end
    end

    records_segment_lengths = map(records_selection) do selection
        seglens = Int[]
        isempty(selection) && return seglens
        last_i = 0
        current_btup = bench_tuples[first(selection)]
        for (i, index) in enumerate(selection)
            if bench_tuples[index] !== current_btup
                current_btup = bench_tuples[index]
                push!(seglens, i - last_i)
                last_i = i + 1
            end
        end
        push!(seglens, length(selection) - last_i)
        seglens
    end

    x_coords = map(records_selection) do sel
        datetime2unix.(DateTime.(ns_data.date[sel]))
    end

    sets = map(x_coords) do _
        subcats = ns_data.subcategory_1[records_selection[]]
        indexin(subcats, unique(subcats))
    end

    # Axis setup

    ax_time = Axis(
        fig[1,1:end],
        ytickformat = time_fmt,
        ylabel = Makie.rich("Minimum execution time",
                            color=colours.time.dark, font=:bold),
        title = "Nanosoldier benchmark trends", titlesize = 28,
        yzoomlock = true, ypanlock = true, yrectzoom = false)
    ax_gctime = Axis(
        fig[1,1:end],
        yaxisposition = :right, ytickformat = pct_fmt,
        ylabel = Makie.rich("Median proportion of execution time spent on GC",
                            color=colours.gctime.dark, font=:bold),
        limits = (nothing, (-0.05, 1.05)),
        yzoomlock = true, ypanlock = true, yrectzoom = false)
    ax_mem  = Axis(
        fig[2,1:end],
        ytickformat = mem_fmt, xtickformat = date_fmt,
        ylabel = Makie.rich("Median memory usage",
                            color=colours.mem.dark, font=:bold),
        yzoomlock = true, ypanlock = true, yrectzoom = false)
    ax_alloc  = Axis(
        fig[2,1:end],
        yaxisposition = :right,
        ylabel = Makie.rich("Median allocations",
                            color=colours.alloc.dark, font=:bold),
        yzoomlock = true, ypanlock = true, yrectzoom = false)

    hidexdecorations!(ax_time)

    hidespines!(ax_gctime)
    hidexdecorations!(ax_gctime)

    hidespines!(ax_alloc)
    hidexdecorations!(ax_alloc)

    linkxaxes!(ax_time, ax_gctime, ax_mem, ax_alloc)

    # Coordinates

    coords_time = map(x_coords) do xs
        hcat(xs, ns_data.time_minimum[records_selection[]])
    end
    coords_gc = map(x_coords) do xs
        hcat(xs, ns_data.gctime_median[records_selection[]] ./
            ns_data.time_median[records_selection[]])
    end
    coords_mem = map(x_coords) do xs
        hcat(xs, ns_data.memory_median[records_selection[]])
    end
    coords_alloc = map(x_coords) do xs
        hcat(xs, ns_data.allocs_median[records_selection[]])
    end

    # Ensure the axis are right

    on(coords_time) do pos
        xs, ys = eachcol(pos)
        reset_limits!(ax_time, yauto = false)
        notify(ax_time.finallimits)
    end

    on(coords_gc) do _
        reset_limits!(ax_gctime, yauto = false)
    end

    on(coords_mem) do pos
        xs, ys = eachcol(pos)
        reset_limits!(ax_mem, yauto = false)
        notify(ax_mem.finallimits)
    end

    on(coords_alloc) do pos
        xs, ys = eachcol(pos)
        reset_limits!(ax_alloc, yauto = false)
        notify(ax_alloc.finallimits)
    end

    # Rescale y axis on zoom

    for (ax, coords, yfactor) in (
        (ax_time, coords_time, 1.05),
        (ax_mem, coords_mem, 1.05),
        (ax_alloc, coords_alloc, 1.2))
        on(ax.finallimits) do rect
            axmin, aymin = rect.origin
            axrange, ayrange = rect.widths
            axmax, aymax = axmin + axrange, aymin + ayrange
            xs, ys = eachcol(coords[])
            visable_ys = ys[axmin .<= xs .<= axmax]
            isempty(visable_ys) && return
            vymax = maximum(visable_ys, init=1)
            vymax_target = yfactor * vymax
            if abs(aymax - vymax_target) > 0.1 * vymax
                ylims!(ax, -0.05 * vymax, vymax_target)
            end
        end
    end

    # Scatter plots

    ax_coord_colour_sets = (
        (ax_time, coords_time, colours.time),
        (ax_gctime, coords_gc, colours.gctime),
        (ax_mem, coords_mem, colours.mem),
        (ax_alloc, coords_alloc, colours.alloc))

    for (ax, coord, colour) in ax_coord_colour_sets
        sctr_points = map(coord, togl_scatter.active) do coord, enabled
            ifelse(enabled, coord, zeros(Float64, 0, 2))
        end
        sctr_colour = map(togl_lines.active) do enabled
            ifelse(enabled, colour.light, colour.base)
        end
        scatter!(ax, sctr_points, color = sctr_colour)
    end

    # Line plots

    for (ax, coord, colour) in ax_coord_colour_sets
        line_points = map(coord, togl_lines.active, togl_smooth.active) do coords, enabled, smooth
            points = Tuple{Float64, Float64}[]
            enabled || return points
            xs, ys = eachcol(coords)
            offset = 0
            for len in records_segment_lengths[]
                segment = 1+offset:len+offset
                offset += len
                xs1, ys1 = xs[segment], ys[segment]
                xperm = sortperm(xs1)
                xs1, ys1 = xs1[xperm], ys1[xperm]
                newpoints = if smooth
                    model = loess(xs1, ys1, span=0.1)
                    us = range(extrema(xs1)..., length=length(xs1))
                    vs = predict(model, us)
                    tuple.(us, vs)
                else
                    tuple.(xs1, ys1)
                end
                append!(points, newpoints)
                push!(points, (NaN, NaN))
            end
            points
        end

        line_colours = map(coord, togl_lines.active) do _, enabled
            colours = Vector{typeof(colour.base)}()
            if enabled
                for len in records_segment_lengths[]
                    append!(colours, fill(colour.base, len+1))
                end
            end
            colours
        end

        lines!(ax, line_points, color = line_colours,
               linewidth=2, depth_shift=Float32(-0.01))
    end

    fig
end
