using SimpleSDMLayers
using GBIF
using Plots
using GLM
using StatsBase
using Statistics
using GeometryBasics

observations = occurrences(
    GBIF.taxon("Hypomyces lactifluorum"; strict=true),
    "hasCoordinate" => "true",
    "country" => "CA",
    "country" => "US",
    "limit" => 300,
)

while length(observations) < size(observations)
    occurrences!(observations)
end

left, right = extrema(longitudes(observations)) .+ (-5, 5)
bottom, top = extrema(latitudes(observations)) .+ (-5, 5)
boundaries = (left=left, right=right, bottom=bottom, top=top)

predictors =
    convert.(
        Float32, SimpleSDMPredictor(WorldClim, BioClim, 1:19; resolution=10.0, boundaries...)
    );

push!(
    predictors,
    convert(
        Float32, SimpleSDMPredictor(WorldClim, Elevation; resolution=10.0, boundaries...)
    ),
);

plot(plot.(predictors, grid=:none, axes=false, frame=:none, leg=false, c=:imola)...)

# here

function vif(model)
    R² = r2(model)
    return 1 / (1-R²)
end

function stepwisevif(
    layers::Vector{T}, selection=collect(1:length(layers)), threshold::Float64=5.0
) where {T<:SimpleSDMLayer}
    x = hcat([layer[keys(layer)] for layer in layers[selection]]...)
    X = (x .- mean(x; dims=1)) ./ std(x; dims=1)
    vifs = zeros(Float64, length(selection))
    for i in eachindex(selection)
        vifs[i] = vif(lm(X[:, setdiff(eachindex(selection), i)], X[:, i]))
    end
    all(vifs .<= threshold) && return selection
    drop = last(findmax(vifs))
    popat!(selection, drop)
    @info "Variables remaining: $(selection)"
    return stepwisevif(layers, selection, threshold)
end

layers_to_keep = stepwisevif(predictors)

plot(
    plot.(
        predictors[layers_to_keep], grid=:none, axes=false, frame=:none, leg=false, c=:imola
    )...,
)

_pixel_score(x) = 2.0(x > 0.5 ? 1.0 - x : x);

presences = mask(predictors[1], observations, Bool)
plot(convert(Float32, presences); c=cgrad([:lightgrey, :black]), leg=false)

function SDM(predictor::T1, observations::T2) where {T1<:SimpleSDMLayer,T2<:SimpleSDMLayer}
    _tmp = mask(observations, predictor)
    qf = ecdf(convert(Vector{Float32}, _tmp[keys(_tmp)])) # We only want the observed values
    return (_pixel_score ∘ qf)
end

function SDM(predictors::Vector{T}, models) where {T<:SimpleSDMLayer}
    @assert length(models) == length(predictors)
    return minimum([broadcast(models[i], predictors[i]) for i in 1:length(predictors)])
end

# here

models = [SDM(predictor, presences) for predictor in predictors];
prediction = SDM(predictors, models)

plot(prediction; c=:bamako, frame=:box)
xaxis!("Longitude")
yaxis!("Latitude")