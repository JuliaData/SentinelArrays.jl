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
    @test argmax(x -> x + 1, x) == 11
    @test argmin(x) == 1
    @test argmin(x -> x + 1, x) == 2
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

    x = ChainedVector(Vector{Float64}[])
    @test !any(x -> iseven(x), x)
    @test !any(map(x -> iseven(x), x))
    @test all(x -> iseven(x), x)
    @test all(map(x -> iseven(x), x))
    @test_throws ArgumentError reduce(+, x)
    @test_throws ArgumentError foldl(+, x)
    @test_throws ArgumentError foldr(+, x)
    @test_throws ArgumentError mapreduce(x -> x + 1, +, x)
    @test_throws ArgumentError mapfoldl(x -> x + 1, +, x)
    @test_throws ArgumentError mapfoldr(x -> x + 1, +, x)
    @test count(x -> iseven(x), x) == 0
    @test count(map(x -> iseven(x), x)) == 0
    @test_throws ArgumentError extrema(x)
    @test_throws ArgumentError extrema(x -> x + 1, x)
    @test_throws ArgumentError minimum(x)
    @test_throws ArgumentError minimum(x -> x + 1, x)
    @test_throws ArgumentError maximum(x)
    @test_throws ArgumentError maximum(x -> x + 1, x)

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
        @test x[aidx] == y[bidx]
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
