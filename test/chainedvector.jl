@testset "ChainedVector" begin

    # identity checks
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
    @test typeof(Base.unaliascopy(x)) == typeof(x)
    @test Base.unaliascopy(x) == x

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

    @test_throws ArgumentError insert!(x, true, 1)
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

    # similar
    x = ChainedVector([[1,2,3], [4,5,6], [7,8,9,10]])
    y = similar(x, length(x))
    @test length(x) == length(y)
    @test eltype(x) == eltype(y)
    y = similar(x, Union{Missing, eltype(x)}, 2 * length(x))
    @test 2 * length(x) == length(y)
    @test eltype(y) == Union{Missing, eltype(x)}
    y = similar(x, 1)
    @test length(y) == 1
    y = similar(x, 0)
    @test length(y) == 0
    @test eltype(x) == eltype(y)
    y2 = similar(y, 10)
    @test length(y2) == 10
    @test eltype(y2) == eltype(x)

    x = ChainedVector([[1,2,3], [4,5,6], [7,8,9,10]])
    @test 10 in x
    @test !(11 in x)
    cnt = Ref(0)
    foreach(x -> cnt[] += 1, x)
    @test cnt[] == length(x)
    y = map(identity, x)
    @test x == y
    y = map(x -> x + 1, x)
    @test all(x -> x[1] == x[2] + 1, zip(y, x))

    # https://github.com/JuliaData/SentinelArrays.jl/issues/57
    cx1 = ChainedVector([[1, 2], [3]])
    cx2 = ChainedVector([[1.1, 2.2], [3.3]])
    @test ChainedVector([cx1, cx2]) isa ChainedVector{Float64}

    x = ChainedVector([[1], [2], [3]])
    y = map(v -> v == 1 ? missing : v, x)
    @test y isa ChainedVector{Union{Missing,Int}}
    @test isequal(y, ChainedVector([[missing], [2], [3]]))

    x = ChainedVector([[1,2,3], [4,5,6], [7,8,9,10]])
    y = map(v -> v == 1 ? missing : v, x)
    @test y isa ChainedVector{Union{Missing,Int}}
    @test isequal(y, ChainedVector([[missing,2,3], [4,5,6], [7,8,9,10]]))

    x = ChainedVector(Vector{Float64}[])
    y = map(v -> v > 1, x)
    @test y isa ChainedVector{Bool}

    # map!
    x = ChainedVector([[1,2,3], [4,5,6], [7,8,9,10]])
    y = copy(x)
    map!(x -> x + 1, y, x)
    @test all(x -> x[1] + 1 == x[2], zip(x, y))
    map!(x -> x + 1, x, y)
    @test all(x -> x[1] + 1 == x[2], zip(y, x))
    map!(x -> 1, x, x)
    @test all(x -> x == 1, x)
    x = ChainedVector([[1,2,3], [4,5,6], [7,8,9,10]])
    y = ChainedVector([[1,2,3], [4,5,6,7], [8,9,10]])
    map!(x -> x + 1, x, y)
    @test all(x -> x[1] + 1 == x[2], zip(y, x))

    # reductions
    x = ChainedVector([[1,2,3], [4,5,6], [7,8,9,10]])
    @test any(x -> iseven(x), x)
    @test any(map(x -> iseven(x), x))
    @test !all(x -> iseven(x), x)
    @test !all(map(x -> iseven(x), x))
    @test reduce(+, x) == 55
    @test foldl(+, x) == 55
    @test foldr(+, x) == 55
    @test mapreduce(x -> x + 1, +, x) == 65
    @test mapfoldl(x -> x + 1, +, x) == 65
    @test mapfoldr(x -> x + 1, +, x) == 65
    @test count(x -> iseven(x), x) == 5
    @test count(map(x -> iseven(x), x)) == 5
    @test extrema(x) == (1, 10)
    @test extrema(x -> x + 1, x) == (2, 11)
    @test minimum(x) == 1
    @test minimum(x -> x + 1, x) == 2
    @test maximum(x) == 10
    @test maximum(x -> x + 1, x) == 11

    # finds
    @test findmax(x) == (10, 10)
    @test findmax(x -> x + 1, x) == (11, 10)
    @test findmin(x) == (1, 1)
    @test findmin(x -> x + 1, x) == (2, 1)
    @test argmax(x) == 10
    @test argmax(x -> x + 1, x) == 10
    @test argmin(x) == 1
    @test argmin(x -> x + 1, x) == 1
    @test argmax(abs, ChainedVector([-10:5])) == -10
    @test findfirst(iseven, x) == 2
    @test findfirst(x -> x == 10, x) == 10
    @test findfirst(x -> x == 11, x) === nothing
    @test findfirst(map(iseven, x)) == 2
    @test findlast(iseven, x) == 10
    @test findlast(x -> x == 1, x) == 1
    @test findlast(x -> x == 11, x) === nothing
    @test findlast(map(iseven, x)) == 10
    @test findnext(iseven, x, 2) == 2
    @test findnext(map(iseven, x), 2) == 2
    @test findnext(iseven, x, 3) == 4
    @test findnext(x -> x == 11, x, 3) === nothing
    @test findprev(iseven, x, 10) == 10
    @test findprev(map(iseven, x), 10) == 10
    @test findprev(iseven, x, 9) == 8
    @test findprev(x -> x == 11, x, 9) === nothing
    @test findall(map(iseven, x)) == [2, 4, 6, 8, 10]
    @test findall(iseven, x) == [2, 4, 6, 8, 10]
    @test filter(iseven, x) == [2, 4, 6, 8, 10]
    filter!(iseven, x)
    @test x == [2, 4, 6, 8, 10]
    @test length(x) == 5
    @test replace(iseven, x) == map(iseven, x)
    @test replace!(iseven, copy(x)) == replace!(iseven, x)
    @test replace(x, 1 => 2) == map(x -> x == 1 ? 2 : x, x)
    @test map(x -> x == 1 ? 2 : x, x) == replace!(x, 1 => 2)
    @test all(==(2), x)
    @test length(x) == 5

    # https://github.com/JuliaData/SentinelArrays.jl/issues/97
    x = ChainedVector([[18, 70, 92, 15, 65], [25, 14, 95, 54, 57]])
    @test findmax(x) == (95, 8)
    @test findmin(x) == (14, 7)
    @test argmax(x) == 8
    @test argmin(x) == 7
    @test findmax(inv, x) == (inv(14), 7)
    @test findmin(inv, x) == (inv(95), 8)
    @test argmax(inv, x) == 14
    @test argmin(inv, x) == 95

    x = ChainedVector(Vector{Float64}[])
    @test !any(x -> iseven(x), x)
    @test !any(map(x -> iseven(x), x))
    @test all(x -> iseven(x), x)
    @test all(map(x -> iseven(x), x))
    @test_throws Union{ArgumentError,MethodError} reduce(+, x)
    @test_throws Union{ArgumentError,MethodError} foldl(+, x)
    @test_throws Union{ArgumentError,MethodError} foldr(+, x)
    @test_throws Union{ArgumentError,MethodError} mapreduce(x -> x + 1, +, x)
    @test_throws Union{ArgumentError,MethodError} mapfoldl(x -> x + 1, +, x)
    @test_throws Union{ArgumentError,MethodError} mapfoldr(x -> x + 1, +, x)
    @test count(x -> iseven(x), x) == 0
    @test count(map(x -> iseven(x), x)) == 0
    @test_throws ArgumentError extrema(x)
    @test_throws ArgumentError extrema(x -> x + 1, x)
    @test_throws Union{ArgumentError,MethodError} minimum(x)
    @test_throws Union{ArgumentError,MethodError} minimum(x -> x + 1, x)
    @test_throws Union{ArgumentError,MethodError} maximum(x)
    @test_throws Union{ArgumentError,MethodError} maximum(x -> x + 1, x)

    @test_throws ArgumentError findmax(x)
    @test_throws ArgumentError findmax(x -> x + 1, x)
    @test_throws ArgumentError findmin(x)
    @test_throws ArgumentError findmin(x -> x + 1, x)
    @test_throws ArgumentError argmax(x)
    @test_throws ArgumentError argmax(x -> x + 1, x)
    @test_throws ArgumentError argmin(x)
    @test_throws ArgumentError argmin(x -> x + 1, x)
    @test findfirst(iseven, x) === nothing
    @test findfirst(x -> x == 10, x) === nothing
    @test findfirst(x -> x == 11, x) === nothing
    @test findfirst(map(iseven, x)) === nothing
    @test findlast(iseven, x) === nothing
    @test findlast(x -> x == 1, x) === nothing
    @test findlast(x -> x == 11, x) === nothing
    @test findlast(map(iseven, x)) === nothing
    @test findnext(iseven, x, 2) === nothing
    @test findnext(iseven, x, 3) === nothing
    @test findnext(x -> x == 11, x, 3) === nothing
    @test findprev(iseven, x, 10) === nothing
    @test findprev(iseven, x, 9) === nothing
    @test findprev(x -> x == 11, x, 9) === nothing
    @test findall(map(iseven, x)) == Union{}[]
    @test findall(iseven, x) == Union{}[]
    @test filter(iseven, x) == Float64[]
    filter!(iseven, x)
    @test isempty(x)
    @test replace(iseven, x) == map(iseven, x)
    @test replace(x, 1 => 2) == map(x -> x == 1 ? 2.0 : x, x)
    @test replace!(iseven, copy(x)) == replace!(iseven, x)
    @test map(x -> x == 1 ? 2.0 : x, x) == replace!(x, 1 => 2)
    @test isempty(x)

    # copyto!
    # ChainedVector dest: doffs, soffs, n
    x = ChainedVector([[1,2,3], [4,5,6], [7,8,9,10]])
    y = copy(map(x -> x + 1, x))
    x2 = similar(x)
    copyto!(x2, y)
    @test x2 == y
    copyto!(x2, 2, y[1:end-1])
    @test x2[1] == y[1]
    @test x2[2:end] == y[1:end-1]
    @test_throws ArgumentError copyto!(x2, 1, y, 1, -1)
    @test_throws ArgumentError copyto!(x2, 1, y, 1, 30)
    copyto!(x2, 2, y, 9, 1)
    @test x2[2] == y[9]
    y2 = copy(x2);
    copyto!(x2, 1, copy(x), 1, 0)
    @test x2 == y2
    copyto!(x2, 2, y2, 2)
    @test x2[2:end] == y2[2:end]
    # ChainedVector src: doffs, soffs, n
    y2 = copy(y);
    copyto!(y2, x)
    @test y2 == x
    copyto!(y2, 2, view(x, 1:(length(x) - 1)))
    @test y2[2:end] == x[1:end-1]
    y3 = copy(y2)
    copyto!(y2, 1, x, 1, 0)
    @test y2 == y3
    @test_throws ArgumentError copyto!(y2, 1, x, 1, 30)
    @test_throws ArgumentError copyto!(y2, 1, x, 1, -1)
    copyto!(y2, 2, x, 1, 1)
    @test y2[2] == x[1]
    y2 = copy(y);
    copyto!(y2, 2, x, 2)
    @test y2[2:end] == x[2:end]
    # ChainedVector dest & src: doffs, soffs, n
    x = ChainedVector([[1,2,3], [4,5,6], [7,8,9,10]])
    x2 = ChainedVector([[1,2,3], [4,5,6], [7,8,9,10]])
    y = map(x -> x + 1, x)
    copyto!(x2, y)
    @test x2 == y
    x2 = map(identity, x)
    copyto!(x2, 2, view(y, 2:length(y)))
    @test x2[2:end] == y[2:end]
    x2 = map(identity, x)
    copyto!(x2, 2, y, 2)
    @test x2[2:end] == y[2:end]
    # https://github.com/JuliaData/SentinelArrays.jl/issues/71
    x = ChainedVector([SentinelArray(["a", "b", "c"]), SentinelArray(["d", "e", "f"])])
    y = similar(x, 10)
    copyto!(y, 4, x)
    @test y[4:9] == x
    for i in [1, 2, 3, 10]
        @test !isassigned(y, i)
    end

    # https://github.com/JuliaData/SentinelArrays.jl/issues/45
    a = []
    append!(a, ChainedVector([[1, 2, 3]]))
    @test length(a) == 3

    # https://github.com/JuliaData/CSV.jl/issues/842
    @test size(eachindex(ChainedVector([["a"]]))) == (1,)

    x = [1, 2, 3]
    append!(x, ChainedVector([[1, 2, 3]]))
    @test length(x) == 6
    @test x == [1, 2, 3, 1, 2, 3]

    x = ChainedVector([[1,2,3], [4,5,6], [7,8,9,10]])
    for (i, idx) in enumerate(eachindex(x))
       @test i == idx
    end

    i1 = first(eachindex(x))
    @test convert(Int,  i1) isa Int
    @test convert(Int8,  i1) isa Int8
    @test i1 == 1
    @test i1 < 2
    @test isless(i1, 2)

    # https://github.com/JuliaData/SentinelArrays.jl/issues/79
    v = ChainedVector([rand(5), rand(5)])
    @test (rand(10,10) * v) isa ChainedVector
end

@testset "ChainedVectors on Generated Vectors" begin
    #=
    # Use to generate text below
    function test_vector_generator(;
        lengths = rand(0:5, 5),
        possible_values =  1:100,
    )
        values = rand(possible_values, sum(lengths))
        remaining_values = copy(values)
        arrays = map(lengths) do length
            result = remaining_values[1:length]
            remaining_values = @view remaining_values[length+1:end]
            return result
        end
        return ChainedVector(arrays) => values
    end
    function Base.show(io::IO, cv::ChainedVector)
        print(io, "ChainedVector(")
        show(io, cv.arrays)
        print(io, ")")
    end
    for i in 1:10
        test_vector_generator() |>
            repr |>
            x->replace(x, "=>" => "=>\n    ") |>
            x->println(x,",")
    end
    =#

    # Pairs of text vectors
    # Some were inspired by https://github.com/JuliaData/SentinelArrays.jl/issues/97
    int_vectors = [
        ChainedVector([[100, 20], [10, 30, 70, 40], [50], Int[], [60, 90, 80]]) =>
            [100, 20, 10, 30, 70, 40, 50, 60, 90, 80],
        ChainedVector([[2,1,3], [4,5,6], [7,8,10,9]]) =>
            [2, 1, 3, 4, 5, 6, 7, 8, 10, 9],
        ChainedVector([[18, 70, 92, 15, 65], [25, 14, 95, 54, 57]]) =>
            [18, 70, 92, 15, 65, 25, 14, 95, 54, 57],
        ChainedVector([[2, 34], [61, 8, 71], [65, 81, 51], [48, 93, 48, 94], [59, 15, 16, 56, 83]]) =>
            [2, 34, 61, 8, 71, 65, 81, 51, 48, 93, 48, 94, 59, 15, 16, 56, 83],
        ChainedVector([[23, 97, 70, 70], [4, 4], [61, 17], [95, 84, 91]]) =>
            [23, 97, 70, 70, 4, 4, 61, 17, 95, 84, 91],
        ChainedVector([[61, 23, 67, 61], [27, 19, 100], [26, 95], [2, 27, 63], [51, 52, 25]]) =>
            [61, 23, 67, 61, 27, 19, 100, 26, 95, 2, 27, 63, 51, 52, 25],
        ChainedVector([[25, 6, 94], [50], [63, 1, 76], [96, 6]]) =>
            [25, 6, 94, 50, 63, 1, 76, 96, 6],
        ChainedVector([[98, 5, 94], [82, 60], [58, 46, 13, 62, 48]]) =>
            [98, 5, 94, 82, 60, 58, 46, 13, 62, 48],
        ChainedVector([[28, 26], [21, 18, 64, 15], [11, 81, 17, 90], [29], [16, 67, 34, 84]]) =>
            [28, 26, 21, 18, 64, 15, 11, 81, 17, 90, 29, 16, 67, 34, 84],
        ChainedVector([[95, 15, 49, 31, 63], [79, 88], [76], [87, 52], [86, 50, 68, 61]]) =>
            [95, 15, 49, 31, 63, 79, 88, 76, 87, 52, 86, 50, 68, 61],
        ChainedVector([[71], [96, 84], [88, 3], [76, 47]]) =>
            [71, 96, 84, 88, 3, 76, 47],
        ChainedVector([[7, 21, 31], [45], [53, 53]]) =>
            [7, 21, 31, 45, 53, 53],
        ChainedVector([[24, 28, 75, 42], [7, 38, 59, 10], [21, 30, 14], [8, 39], [13, 68, 42]]) =>
            [24, 28, 75, 42, 7, 38, 59, 10, 21, 30, 14, 8, 39, 13, 68, 42],
    ]
    floating_point_vectors = [
        ChainedVector([[2.1, -4.6, -2.5], [-5.0, 6.4, 2.0, -0.5], [-6.1, -7.6, -3.2, -4.7, 4.3], [-1.7, 6.4, -8.9, -7.4], [-7.7, -1.4, 3.1, 4.5]]) =>
            [2.1, -4.6, -2.5, -5.0, 6.4, 2.0, -0.5, -6.1, -7.6, -3.2, -4.7, 4.3, -1.7, 6.4, -8.9, -7.4, -7.7, -1.4, 3.1, 4.5],
        ChainedVector([[-8.5, -1.2, -3.8, 7.5], [8.2, 7.5, -5.3], [-2.7, 0.6, -6.2, 6.1, 1.4]]) =>
            [-8.5, -1.2, -3.8, 7.5, 8.2, 7.5, -5.3, -2.7, 0.6, -6.2, 6.1, 1.4],
        ChainedVector([[-7.2], [8.1, 2.3, 7.5], [-8.4, -5.7]]) =>
            [-7.2, 8.1, 2.3, 7.5, -8.4, -5.7],
        ChainedVector([[-3.7, 7.8, -5.0], [0.1], [5.0, -4.1], [-1.6, -0.9, 8.7, -7.8]]) =>
            [-3.7, 7.8, -5.0, 0.1, 5.0, -4.1, -1.6, -0.9, 8.7, -7.8],
        ChainedVector([[8.6, -2.0], [8.0, 3.4, 3.3], [1.0], [5.4, -2.6, -4.7, 4.4, 4.4], [7.9]]) =>
            [8.6, -2.0, 8.0, 3.4, 3.3, 1.0, 5.4, -2.6, -4.7, 4.4, 4.4, 7.9],
        ChainedVector([[7.6, 5.9], [7.9, -8.8, -1.5, -0.4, 6.0], [-5.1, -0.4, 4.4, 7.3]]) =>
            [7.6, 5.9, 7.9, -8.8, -1.5, -0.4, 6.0, -5.1, -0.4, 4.4, 7.3],
        ChainedVector([[3.2, -3.2, 1.2, -1.2, -2.1], [0.5], [6.2], [2.9], [-8.1, 5.8, 4.8, -3.4, -3.1]]) =>
            [3.2, -3.2, 1.2, -1.2, -2.1, 0.5, 6.2, 2.9, -8.1, 5.8, 4.8, -3.4, -3.1],
        ChainedVector([[-8.0, -1.9, -5.1, -1.4, -8.3], [5.1, -3.7, 6.3, -4.8, -3.3], [-7.0], [-2.4, 4.0, -3.7], [-6.6, -6.9, 2.5, -1.3]]) =>
            [-8.0, -1.9, -5.1, -1.4, -8.3, 5.1, -3.7, 6.3, -4.8, -3.3, -7.0, -2.4, 4.0, -3.7, -6.6, -6.9, 2.5, -1.3],
        ChainedVector([[-7.5], [-1.5, -5.8, 8.4], [-8.4, -1.9, 2.3, -0.8, -8.5], [0.2, 0.5, -7.4, 2.1, -3.9]]) =>
            [-7.5, -1.5, -5.8, 8.4, -8.4, -1.9, 2.3, -0.8, -8.5, 0.2, 0.5, -7.4, 2.1, -3.9],
        ChainedVector([[3.9, -8.9], [-0.3, 0.0, 7.3], [-2.9, 8.6, 5.8, 0.5], [0.0, -4.5, 3.3, 0.4, -3.2]]) =>
            [3.9, -8.9, -0.3, 0.0, 7.3, -2.9, 8.6, 5.8, 0.5, 0.0, -4.5, 3.3, 0.4, -3.2],
    ]
    rational_vectors = [
        ChainedVector(Vector{Rational{Int64}}[[1, 1//2, 1//2, 4//5], [7//10], [1, 1//5, 7//10, 3//10, 1], [3//5], [1]]) =>
            Rational{Int64}[1, 1//2, 1//2, 4//5, 7//10, 1, 1//5, 7//10, 3//10, 1, 3//5, 1],
        ChainedVector(Vector{Rational{Int64}}[[1//5], [1, 4//5, 1//5], [3//5, 7//10, 3//5], [9//10, 1//5, 7//10, 1//2], [1//2, 7//10, 9//10, 3//5, 7//10]]) =>
            Rational{Int64}[1//5, 1, 4//5, 1//5, 3//5, 7//10, 3//5, 9//10, 1//5, 7//10, 1//2, 1//2, 7//10, 9//10, 3//5, 7//10],
        ChainedVector(Vector{Rational{Int64}}[[7//10, 1, 1//5, 1//2, 2//5], [1//5, 4//5, 1//2, 1//5], [3//10, 3//10, 1//2], [3//10, 1//10, 4//5, 3//5], [2//5, 7//10, 1, 3//10, 3//10]]) =>
            Rational{Int64}[7//10, 1, 1//5, 1//2, 2//5, 1//5, 4//5, 1//2, 1//5, 3//10, 3//10, 1//2, 3//10, 1//10, 4//5, 3//5, 2//5, 7//10, 1, 3//10, 3//10],
        ChainedVector(Vector{Rational{Int64}}[[1//10, 4//5], [1//2], [1//10], [4//5, 1, 3//5, 9//10, 9//10]]) =>
            Rational{Int64}[1//10, 4//5, 1//2, 1//10, 4//5, 1, 3//5, 9//10, 9//10],
        ChainedVector(Vector{Rational{Int64}}[[3//10, 1, 9//10, 3//5], [1, 1], [1, 4//5, 3//5, 9//10]]) =>
            Rational{Int64}[3//10, 1, 9//10, 3//5, 1, 1, 1, 4//5, 3//5, 9//10],
        ChainedVector(Vector{Rational{Int64}}[[3//10, 7//10], [4//5], [4//5, 1, 1//10, 9//10], [1, 1, 4//5]]) =>
            Rational{Int64}[3//10, 7//10, 4//5, 4//5, 1, 1//10, 9//10, 1, 1, 4//5],
        ChainedVector(Vector{Rational{Int64}}[[2//5], [3//5, 9//10, 7//10, 9//10], [1//2, 1, 1//10, 1//5], [1//5, 4//5, 7//10, 2//5]]) =>
            Rational{Int64}[2//5, 3//5, 9//10, 7//10, 9//10, 1//2, 1, 1//10, 1//5, 1//5, 4//5, 7//10, 2//5],
        ChainedVector(Vector{Rational{Int64}}[[7//10], [3//5, 1//5, 2//5, 3//5, 4//5], [4//5], [7//10, 3//5, 7//10, 7//10, 1//10]]) =>
            Rational{Int64}[7//10, 3//5, 1//5, 2//5, 3//5, 4//5, 4//5, 7//10, 3//5, 7//10, 7//10, 1//10],
        ChainedVector(Vector{Rational{Int64}}[[1//2, 1, 1//2, 9//10, 2//5], [9//10, 1//2, 3//5], [4//5, 7//10], [3//10, 2//5], [9//10, 1]]) =>
            Rational{Int64}[1//2, 1, 1//2, 9//10, 2//5, 9//10, 1//2, 3//5, 4//5, 7//10, 3//10, 2//5, 9//10, 1],
        ChainedVector(Vector{Rational{Int64}}[[9//10, 3//10, 1//10, 2//5], [4//5], [9//10, 2//5]]) =>
            Rational{Int64}[9//10, 3//10, 1//10, 2//5, 4//5, 9//10, 2//5],

    ]
    @testset for (x,y) in Iterators.flatten([int_vectors, floating_point_vectors, rational_vectors])
        @test copy(x) == y
        @test collect(x) == y
        @test length(x) == length(y)
        # should this be approx?
        @test sum(x) ≈ sum(y)
        @test findmax(x) == findmax(y)
        @test findmin(x) == findmin(y)
        @test maximum(x) == maximum(y)
        @test minimum(x) == minimum(y)
        @test argmax(x) == argmax(y)
        @test argmin(x) == argmin(y)
        @test all(>(0),x) == all(>(0),y)
        @test any(>(0),x) == any(>(0),y)
        @test any(<(0),x) == any(<(0),y)
        @test count(>(0),x) == count(>(0),y)
        @test count(<(0),x) == count(<(0),y)
        @test extrema(inv, x) == extrema(inv, y)
        @static if VERSION ≥ v"1.6"
            @test findmax(x->x+1, x) == findmax(x->x+1, y)
            @test findmin(x->x-1, x) == findmin(x->x-1, y)
            @test findfirst(isodd, x) == findfirst(isodd, y)
            @test findfirst(iseven, x) == findfirst(iseven ,y)
            @test findlast(isodd, x) == findlast(isodd, y)
            @test findlast(iseven, x) == findlast(iseven ,y)
            @test findall(iseven, x) == findall(iseven ,y)
            @test findnext(isodd, x, 5) == findnext(isodd, y, 5)
            @test findprev(isodd, x, 5) == findprev(isodd, y, 5)
        end
        @test let (val, idx) = findmax(x)
            max_val = maximum(x)
            val == max_val == x[idx]
        end
        @test let (val, idx) = findmin(x)
            min_val = minimum(x)
            val == min_val == x[idx]
        end
        @test x[argmax(x)] == maximum(x)
        @test x[argmin(x)] == minimum(x)
        @test let (val, idx) = findmax(inv, x)
            max_val = maximum(inv, x)
            val == max_val == inv(x[idx])
        end
        @test let (val, idx) = findmin(inv, x)
            min_val = minimum(inv, x)
            val == min_val == inv(x[idx])
        end
        @test inv(argmax(inv, x)) == maximum(inv, x)
        @test inv(argmin(inv, x)) == minimum(inv, x)
    end
end


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
    x = ChainedVector([collect(1:i) for i = 10:100])
    y = copy(x)
    for (a, b) in zip(x, y)
        @test a == b
    end
    for (aidx, bidx) in zip(eachindex(x), eachindex(y))
        # getindex w/ custom ChainedVectorIndex
        @test aidx == bidx
        @test x[aidx] == y[bidx]
        @test hash(aidx) == hash(bidx)
    end
    for (i, idx) in enumerate(eachindex(x))
        # setindex! w/ custom ChainedVectorIndex
        x[idx] = i
    end
    @test x == 1:length(x)
    ei = eachindex(x)
    @test length(ei) == length(x)
    @test eltype(ei) <: SentinelArrays.ChainedVectorIndex
    idx = collect(Iterators.take(ei, 20))
    @test x[1:20] == x[idx]
    x = ChainedVector(Vector{Int}[])
    @test isempty(x)
    @test length(x) == 0
    cnt = 0
    for y in x
        cnt += 1
    end
    @test cnt == 0
    for i in eachindex(x)
        cnt += x[i]
    end
    @test cnt == 0
end

@testset "empty ChainedVector" begin
    x = ChainedVector([[]])
    @test isempty(x)
    @test length(x) == 0
    @test_throws BoundsError x[1]
    @test_throws BoundsError x[end]

    push!(x, 1)
    @test x[end] == 1

    empty!(x)
    append!(x, [2])
    @test x[end] == 2
end

@testset "prevind/nextind ChainedVector" begin
    x = ChainedVector([collect(1:i) for i = 10:100])
    ind = first(eachindex(x))
    for i = 1:length(x)
        @test x[ind] == x[i]
        ind = nextind(x, ind)
    end
    @test_throws BoundsError x[ind]
    for i = length(x):-1:1
        ind = prevind(x, ind)
        @test x[ind] == x[i]
    end
    ind = prevind(x, ind)
    @test_throws BoundsError x[ind]
    # https://github.com/apache/arrow-julia/issues/418
    y = ChainedVector([collect(1:i) for i = 10:100])
    @test_throws AssertionError x[first(eachindex(y))]
    # https://github.com/JuliaData/SentinelArrays.jl/issues/74
    x = ChainedVector([[true], [false], [true]])
    @test BitVector(x) == [true, false, true]
end

@testset "MissingVector resizing" begin
    v = MissingVector(1)
    @test isequal(push!(v, missing), [missing, missing])
    @test isequal(pushfirst!(v, missing), [missing, missing, missing])
    @test isequal(insert!(v, 2, missing), [missing, missing, missing, missing])
    @test_throws ArgumentError push!(v, 1)
    @test_throws ArgumentError pushfirst!(v, 1.0)
    @test_throws ArgumentError insert!(v, 2, [missing])
end

@testset "deleteat! with Bool mask" begin
    x = SentinelArray(["a", "b", "c", "d", "e"])
    mask = [false, true, false, false, false]
    @test deleteat!(x, mask) == ["a", "c", "d", "e"]
    
    for i in 1:100
        v1 = collect(string.(1:i))
        v2 = copy(v1)
        s1 = SentinelArray(copy(v1))
        s2 = SentinelArray(copy(v1))
        m1 = rand(Bool, i)
        m2 = BitVector(m1)
        @test deleteat!(v1, m1) == deleteat!(s1, m1)
        @test deleteat!(v2, m2) == deleteat!(s2, m2)
    end
end
