
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

end