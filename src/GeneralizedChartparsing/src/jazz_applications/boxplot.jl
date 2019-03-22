function boxplot(data, outputfile="", model_names=0:length(data)-1)
    # data must be an iterble object of lists of numbers

    @assert length(model_names) == length(data)
    n = length(model_names)
    for vals in data
        @assert length(data[1]) == length(vals)
    end
    k = length(data[1])

    df = DataFrame(
        ids = collect(Base.flatten([1:length(vals) for vals in data])),
        value = collect(Base.flatten(data)),
        model = collect(Base.flatten([[name for i in 1:k] for name in model_names]))
    )
    if outputfile == ""
        plot(df, x="model", y="value", Geom.boxplot)
    else
        draw(SVG(outputfile*".svg", n*5cm, 15cm), plot(df, x="model", y="value", Geom.boxplot))
    end
end
