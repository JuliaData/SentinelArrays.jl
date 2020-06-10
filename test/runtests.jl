using SentinelArrays, Test

@testset "SentinelArrays" begin

x = SentinelVector{Float64}(undef, 10)
fill!(x, missing)
@test length(x) == 10
@test all(isequal.(x.data, x.sentinel))

@test x[1] === missing
# force recoding
sent = x.sentinel
x[1] = x.sentinel
@test x[1] === sent

@test size(x) == (10,)
x[1] = 3.14
@test x[1] === 3.14

resize!(x, length(x) + 1)
@test x[end] === missing

x = SentinelVector{Int64}(undef, 1)
x[1] = missing
@test x[1] === missing

x = SentinelVector{Union{Bool, Missing}}(undef, 1, missing, missing)
@test x[1] === missing

x = SentinelVector{String}(undef, 10)
@test x[1] === missing
x[1] = "hey"
@test x[1] == "hey"
x[1] = missing
@test x[1] === missing

@test all(x .=== copy(x))
@test length(empty!(x)) == 0
@test length(resize!(x, 10)) == 10
@test x[1] === missing

push!(x, missing)
@test x[end] === missing
push!(x, "ho")
@test x[end] == "ho"

deleteat!(x, length(x))
@test x[end] === missing
@test length(x) == 11

empty!(x)
append!(x, ["hey", "ho", missing])
@test length(x) == 3
@test isequal(x, ["hey", "ho", missing])

pushfirst!(x, missing)
@test x[1] === missing
prepend!(x, [missing, "first"])
@test x[1] === missing
@test x[2] == "first"

@test popfirst!(x) === missing
@test x[1] == "first"

insert!(x, 1, missing)
@test x[1] === missing
insert!(x, length(x) + 1, "pirate")
@test x[end] == "pirate"

@test splice!(x, length(x)) == "pirate"

end # @testset