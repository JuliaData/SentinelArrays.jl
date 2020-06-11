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

t = [SentinelVector{Int64}(undef, 10), SentinelVector{Int64}(undef, 10), SentinelVector{Int64}(undef, 5)]
sent = t[1].sentinel
@test all(x->x.sentinel == sent, t)
SentinelArrays.newsentinel!(t...; force=false)
# make sure nothing got recoded
@test all(x->x.sentinel == sent, t)
SentinelArrays.newsentinel!(t...; force=true)
# make sure all got recoded
@test all(x->x.sentinel != sent, t)

# force recode of just one vector
sent = t[1].sentinel
t[2][1] = t[2].sentinel
@test !all(x->x.sentinel == sent, t)
SentinelArrays.newsentinel!(t...)
# make sure all got recoded to same
@test all(x->x.sentinel == t[1].sentinel, t)

A = SentinelArray(Int64[i for i = 1:10])
B = SentinelVector{Int64}(undef, 10)

C = vcat(A, B)
@test C[1:10] == collect(1:10)
@test all(C[11:20] .=== missing)

append!(A, B)
@test A[1:10] == collect(1:10)
@test all(A[11:20] .=== missing)

A = SentinelArray(Int64[i for i = 1:10])
B = SentinelVector{Int64}(undef, 10)
# force B to recode
B[1] = B.sentinel
B[1] = missing

C = vcat(A, B)
@test C[1:10] == collect(1:10)
@test all(C[11:20] .=== missing)

append!(A, B)
@test A[1:10] == collect(1:10)
@test all(A[11:20] .=== missing)

# make sure we bail when can't find a new automatic sentinel
A = SentinelArray([i for i = 0x00:0xff])
@test_throws SentinelCollisionError setindex!(A, A.sentinel, 1)

@test_throws ErrorException SentinelVector{Bool}(undef, 1)

t = SentinelVector{Tuple{Int32, Int32}}(undef, 1)
@test t.data[1] === t.sentinel

t = SentinelMatrix{Float64}(undef, (10, 10))
@test size(t) == (10, 10)

end # @testset