using SentinelArrays, Test

@testset "SentinelArrays" begin

x = SentinelVector{Int}(undef, 10)
fill!(x, missing)
@test length(x) == 10
@test all(isequal.(x.data, x.sentinel))

@test x[1] === missing
# force recoding
sent = x.sentinel
x[1] = x.sentinel
@test x[1] === sent

@test size(x) == (10,)
x[1] = 3
@test x[1] === 3

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

@testset "ChainedVector" begin

x = ChainedVector([[1,2,3], [4,5,6], [7,8,9,10]])
@test x == 1:10
@test length(x) == 10

x[1] = 0
x[end] = 11
@test x[1] == 0
@test x[end] == 11

@test copy(x) == x
empty!(x)
@test length(x) == 0
@test copy(x) == x

@test_throws ArgumentError resize!(x, -1)
resize!(x, 10)
@test length(x) == 10
resize!(x, 20)
@test length(x) == 20
resize!(x, 15)
@test length(x) == 15
resize!(x, 5)
@test length(x) == 5

push!(x, 1)
@test x[end] == 1
empty!(x)
push!(x, 1)
@test x[1] == x[end] == 1

pushfirst!(x, 2)
@test x[1] == 2
empty!(x)
pushfirst!(x, 2)
@test x[1] == x[end] == 2
pushfirst!(x, 3)
@test x[1] == 3

@test pop!(x) == 2
@test popfirst!(x) == 3
@test isempty(x)

@test_throws BoundsError insert!(x, 0, 1)
@test_throws BoundsError insert!(x, 2, 1)
insert!(x, 1, 1)
@test x[1] == 1
insert!(x, 1, 2)
@test x[1] == 2

x = ChainedVector([[1,2,3], [4,5,6], [7,8,9,10]])
y = ChainedVector([[11,12,13], [14,15,16], [17,18,19,20]])

z = vcat(x, y)
@test length(z) == 20
@test z == 1:20

z = ChainedVector([[21,22,23], [24,25,26], [27,28,29,30]])
a = vcat(x, y, z)
@test length(a) == 30
@test a == 1:30

empty!(x)
z = vcat(x, y)
@test z == y

x = ChainedVector([[1,2,3], [4,5,6], [7,8,9,10]])
append!(x, y)
@test length(x) == 20
@test x == 1:20

x = ChainedVector([[1,2,3], [4,5,6], [7,8,9,10]])
append!(x, collect(11:20))
@test length(x) == 20
@test x == 1:20

x = ChainedVector([[1,2,3], [4,5,6], [7,8,9,10]])
append!(x, 11:20)
@test length(x) == 20
@test x == 1:20

empty!(x)
append!(x, y)
@test x == y

x = ChainedVector([[1,2,3], [4,5,6], [7,8,9,10]])
y = ChainedVector([[11,12,13], [14,15,16], [17,18,19,20]])

prepend!(y, x)
@test length(y) == 20
@test y == 1:20

y = ChainedVector([[11,12,13], [14,15,16], [17,18,19,20]])
prepend!(y, collect(1:10))
@test length(y) == 20
@test y == 1:20

y = ChainedVector([[11,12,13], [14,15,16], [17,18,19,20]])
prepend!(y, 1:10)
@test length(y) == 20
@test y == 1:20

empty!(y)
prepend!(y, x)
@test y == x

x = ChainedVector([[1,2,3], [4,5,6], [7,8,9,10]])
deleteat!(x, 1)
@test x[1] == 2
deleteat!(x, 1:4)
@test x == 6:10
deleteat!(x, [2, 4])
@test x == [6, 8, 10]

end