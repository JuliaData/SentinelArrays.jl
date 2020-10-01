using SentinelArrays, Test, Random

@testset "SentinelArrays" begin

# this is a very rare corner case
# we put this test first because it relies on
# the seeded random number generator for thread 1
# the issue is if we had an underlying array of non-sentinel values
# then tried to `setindex!` to the _NEXT_ chosen sentinel value
# then the element getting set would end up `missing` (because it == the sentinel value)
# instead of the sentinel value getting cycled to something else
Random.seed!(SentinelArrays.RNG[1], 0)
x = SentinelVector{Int64}(undef, 1)
x[1] = 1
x.sentinel = 1369352191816061504
x[1] = 1369352191816061504
@test x[1] == 1369352191816061504

x = SentinelVector{Int}(undef, 10)
fill!(x, missing)
@test length(x) == 10
@test all(isequal.(x.data, x.sentinel))

@test x[1] === missing
# force recoding
sent = x.sentinel
x[1] = x.sentinel
@test x[1] === sent
r = reverse(x)
@test x.sentinel == r.sentinel

@test size(x) == (10,)
x[1] = 3
@test x[1] === 3

resize!(x, length(x) + 1)
@test x[end] === missing

x = SentinelVector{Int64}(undef, 1)
x[1] = missing
@test x[1] === missing

x = SentinelArray{Float64, 2}(undef, 10, 10)
@test size(x) == (10, 10)
x = SentinelArray{Float64}(undef, 10, 10)
@test size(x) == (10, 10)

x = SentinelArray(fill(3.0, 10, 10))
y = convert(SentinelArray{Int64}, x)
@test size(y) == (10, 10)
@test y isa SentinelArray{Int64}
@test all(y .=== Int64(3))

y = convert(SentinelVector{Int64}, x)
@test size(y) == (10, 10)
@test y isa SentinelArray{Int64}
@test all(y .=== Int64(3))

y = convert(SentinelArray, fill(Int64(3), 10, 10))
@test size(y) == (10, 10)
@test size(y, 1) == 10
@test axes(y, 1) == Base.OneTo(10)
@test stride(y, 1) == 1
@test strides(y) == (1, 10)
@test y isa SentinelArray{Int64}
@test all(y .=== Int64(3))

x = SentinelVector{Union{Bool, Missing}}(undef, 1, missing, missing)
@test x[1] === missing
x[1] = true
@test x[1] === true
x[1] = missing
@test x[1] === missing

x = SentinelArray(["$i" for i = 1:10])
deleteat!(x, [9, 10])
@test length(x) == 8
@test x == ["$i" for i = 1:8]
deleteat!(x, [true, false, true, false, true, false, true, false])
@test length(x) == 4
@test x == ["2", "4", "6", "8"]

x = SentinelVector{String}(undef, 10)
@test x[1] === missing
x[1] = "hey"
@test x[1] == "hey"
x[1] = missing
@test x[1] === missing

@test SentinelArrays.newsentinel!(x) === nothing

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

@test splice!(x, length(x), ["pirate2"]) == "pirate"
@test splice!(x, length(x)) == "pirate2"
@test splice!(x, length(x), ["pirate3", "pirate4"]) === missing

@test splice!(x, (length(x)-1):length(x), ["pirate5", "pirate6"]) == ["pirate3", "pirate4"]
@test splice!(x, (length(x)-1):length(x), ["pirate7"]) == ["pirate5", "pirate6"]
@test splice!(x, length(x), ["pirate8", "pirate9"]) == "pirate7"
@test splice!(x, (length(x)-1):length(x), ["pirate10", "pirate11", "pirate12"]) == ["pirate8", "pirate9"]
@test splice!(x, (length(x)-2):length(x)) == ["pirate10", "pirate11", "pirate12"]

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

t = SentinelArray(collect(1:10))
pushfirst!(t, 3, 2, 1)
@test t[1:3] == [3, 2, 1]

prepend!(t, (i for i = 1:3 if i > 0))
@test t[1:3] == [1, 2, 3]

@test pop!(t) == 10
empty!(t)
@test_throws ArgumentError pop!(t)
@test_throws ArgumentError popfirst!(t)

x = SentinelArray(ones(3))

# broadcasting
@test x .+ x == 2 * ones(3)
@test x .+ x .+ x == 3 * ones(3)
@test (x .= 0 .* x .+ 7) == 7 * ones(3)

v = SentinelArray(zeros(3,))
m = SentinelArray(ones(3, 3))
s = 0

@test v .+ m == ones(3, 3) == m .+ v
@test s .+ m == ones(3, 3) == m .+ s
@test s .+ v .+ m == ones(3, 3) == m .+ s .+ v

casts = (
    SentinelArray,  # Named Matrix
    x->SentinelArray(x[:, 1]),  # Named Vector
    x->SentinelArray(x[:, 1:1]),  # Named Single Column Matrix
    identity, # Matrix
    x->x[:, 1], # Vector
    x->x[:, 1:1], # Single Column Matrix
    first, # Scalar
    )
for (T1, T2, T3) in Iterators.product(casts, casts, casts)
    all(isequal(identity), (T1, T2, T3)) && continue
    !any(isequal(SentinelArray), (T1, T2, T3)) && continue

    total = T1(ones(3, 6)) .+ T2(2ones(3, 6)) .+ T3(3ones(3, 6))
    @test total == 6ones(3, 6)
end

s = SentinelVector{Int}(undef, 2)
@test isequal(max.(s, 0), [missing, missing])
@test (s .=== missing) isa BitArray

s = SentinelArray(collect(1:5))
c = ChainedVector([s, s, s])
c2 = copy(c)
@test length(c) == length(c2)
@test c2 isa SentinelVector

# deleteat! of SentinelArray w/ underlying ChainedVector w/ UndefInitializer
x = SentinelArray(ChainedVector([Vector{String}(undef, 5)]))
deleteat!(x, 1)
@test length(x) == 4

end # @testset

@testset "ChainedVector" begin

x = ChainedVector([[1,2,3], [4,5,6], [7,8,9,10]])
@test x == 1:10
@test length(x) == 10
@test Base.IndexStyle(x) == Base.IndexLinear()

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
@test_throws ArgumentError pop!(x)
@test_throws ArgumentError popfirst!(x)

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

x = ChainedVector([[1,2,3], [4,5,6], [7,8,9,10]])
deleteat!(x, 1)
deleteat!(x, 1)
deleteat!(x, 1)
@test length(x.arrays) == 2

x = ChainedVector([[1,2,3], [4,5,6], [7,8,9,10]])
deleteat!(x, 4)
deleteat!(x, 4)
deleteat!(x, 4)
@test length(x.arrays) == 2

x = ChainedVector([[1,2,3], [4,5,6], [7,8,9,10]])
y = 0
for i in x
    y += i
end
@test y == 55

x = ChainedVector([[1,2,3], [4,5,6], [7,8,9,10]])
b = [true, false, false, false, false, false, false, false, false, false]
deleteat!(x, b)
@test x[1] == 2
@test length(x) == 9

deleteat!(x, Int[])
@test length(x) == 9

#30
x = ChainedVector([Vector{String}(undef, 3), ["hey", "ho"]])
@test !isassigned(x, 1)
@test isassigned(x, 4)

#36
x = ChainedVector([[1,2,3], [4,5,6], [7,8,9], [10,11,12]])
deleteat!(x, [1, 2, 10, 11])
@test x == [3, 4, 5, 6, 7, 8, 9, 12]

#38
A = ChainedVector([collect(((i - 1) * 153 + 1):(i * 153 + 1)) for i = 1:16])
inds = collect(1:1883)
deleteat!(A, inds)
@test length(A) == 581

end

@testset "MissingVector" begin

x = MissingVector(10)
@test all(x .=== missing)
@test length(x) == 10
@test length(x) isa Int
@test Base.IndexStyle(x) == Base.IndexLinear()

y = similar(x, Missing, 5)
@test length(y) == 5

y = empty(x)
@test length(y) == 0

x[1] = missing
x[end] = missing
@test x[1] === missing
@test x[end] === missing

y = similar(x)
@test typeof(y) == MissingVector
@test length(y) == 10
y = similar(x, 20)
@test typeof(y) == MissingVector
@test length(y) == 20

@test isequal(copy(x), x)
empty!(x)
@test length(x) == 0
@test isequal(copy(x), x)

@test_throws ArgumentError resize!(x, -1)
resize!(x, 10)
@test length(x) == 10

push!(x, missing)
@test x[end] === missing
empty!(x)
push!(x, missing)
@test x[1] === x[end] === missing

pushfirst!(x, missing)
@test x[1] === missing
empty!(x)
pushfirst!(x, missing)
@test x[1] === x[end] === missing
pushfirst!(x, missing)

@test pop!(x) === missing
@test popfirst!(x) === missing
@test isempty(x)

@test_throws BoundsError insert!(x, 0, missing)
@test_throws BoundsError insert!(x, 2, missing)
insert!(x, 1, missing)
@test x[1] === missing
insert!(x, 1, missing)
@test x[1] === missing

x = MissingVector(10)
y = MissingVector(10)

z = vcat(x, y)
@test length(z) == 20

empty!(x)
z = vcat(x, y)
@test isequal(z, y)

x = MissingVector(10)
append!(x, y)
@test length(x) == 20

x = MissingVector(10)
append!(x, (missing for _ = 1:10))
@test length(x) == 20

empty!(x)
append!(x, y)
@test isequal(x, y)

x = MissingVector(10)
y = MissingVector(10)

prepend!(y, x)
@test length(y) == 20

y = MissingVector(10)
prepend!(y, (missing for _ = 1:10))
@test length(y) == 20

empty!(y)
prepend!(y, x)
@test isequal(y, x)

x = MissingVector(10)
deleteat!(x, 1)
@test x[1] === missing
deleteat!(x, 1:4)
@test length(x) == 5
deleteat!(x, [2, 4])
@test length(x) == 3

m = similar(x)
@test length(m) == length(x)
m = similar(x, Missing)
@test length(m) == length(x)
@test typeof(m[1:3]) == typeof(m)

deleteat!(x, [true, true, false])
@test length(x) == 1
empty!(x)
@test_throws ArgumentError pop!(x)
@test_throws ArgumentError popfirst!(x)

m = MissingVector(5)
c = ChainedVector([m, m, m])
c2 = copy(c)
@test length(c) == length(c2)
@test c2 isa MissingVector

deleteat!(c2, Int[])
@test length(c2) == 15

@testset "iteration protocol on ChainedVector" begin
    for len in 0:6
        cv = ChainedVector([1:len])
        @test length(cv) == len
        c = 0
        for (i, v) in enumerate(cv)
            c += 1
            @test i == v
        end
        @test c == len
        for j in 0:len
            cv = ChainedVector([1:j, j+1:len])
            @test length(cv) == len
            c = 0
            for (i, v) in enumerate(cv)
                c += 1
                @test i == v
            end
            @test c == len
            for k in j:len
                cv = ChainedVector([1:j, j+1:k, k+1:len])
                @test length(cv) == len
                c = 0
                for (i, v) in enumerate(cv)
                    c += 1
                    @test i == v
                end
                @test c == len

                for l in k:len
                    cv = ChainedVector([1:j, j+1:k, k+1:l, l+1:len])
                    @test length(cv) == len
                    c = 0
                    for (i, v) in enumerate(cv)
                        c += 1
                        @test i == v
                    end
                    @test c == len
                end
            end
        end
    end
end

end
