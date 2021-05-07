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
