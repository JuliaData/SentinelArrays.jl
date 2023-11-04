using Test
using SentinelArrays
using SentinelArrays: BufferedVectors


@noinline unsafe(bv, i) = @inbounds bv[i]

@testset "BufferedVectors" begin
    bv = BufferedVector{Int}()
    @test eltype(bv) == Int
    @test bv == Int[]
    @test bv.elements == Int[]
    @test bv.occupied == 0
    @test length(bv) == 0
    @test length(bv.elements) == 0
    @test isempty(bv)
    @test_throws BoundsError bv[1]
    @test_throws BoundsError first(bv)
    @test_throws BoundsError last(bv)
    @test collect(bv) == Int[]

    Base.ensureroom(bv, 2)
    @test length(bv.elements) == 2
    @test bv.occupied == 0
    @test length(bv) == 0
    @test_throws BoundsError first(bv)

    bv = BufferedVector{Int}([1,2,3], 3)
    @test bv == [1,2,3]
    @test bv.elements[1:3] == [1,2,3]
    @test bv.occupied == 3
    @test length(bv) == 3
    @test size(bv) == (3,)
    @test collect(bv) == [1,2,3]
    @test first(bv) == 1
    @test bv[1] == 1
    @test bv[2] == 2
    @test bv[3] == 3
    @test last(bv) == 3
    @test_throws BoundsError bv[0]
    @test_throws BoundsError bv[4]
    @test !isempty(bv)

    empty!(bv)
    @test bv == Int[]
    @test isempty(bv)

    push!(bv, 1)
    @test bv == [1]
    @assert length(bv) == 1
    skip_element!(bv)
    @test length(bv) == 2

    bv = BufferedVector{Int}()
    push!(bv, 1)
    @test length(bv.elements) == BufferedVectors._grow_by(Int)
    @assert BufferedVectors._grow_by(Int) > 0
    @test_throws BoundsError bv[2]
    unsafe_push!(bv, 42)
    @test bv == [1, 42]

    # --check-bounds=auto not available prior to Julia 1.8, so we force the default
    # bounds check behavior by removing the flag from the command line
    julia_cmd_default_boundscheck = Base.julia_cmd()
    filter!(x->!startswith(x, "--check-bounds="), julia_cmd_default_boundscheck.exec)
    code = """
    using SentinelArrays
    using Test
    bv = BufferedVector{Int}(Int[1, 2], 0)
    # `unsafe` is actually safe here, it's just accessing undef values
    @noinline unsafe(bv, i) = @inbounds bv[i]
    @test unsafe(bv, 2) == bv.elements[2]
    """
    cmd = `$(julia_cmd_default_boundscheck) --startup-file=no --project=. -e $code`
    @test success(pipeline(cmd; stdout=stdout, stderr=stderr))

    bv = BufferedVector{Int32}()
    push!(bv, Int32(1))
    @test length(bv.elements) == BufferedVectors._grow_by(Int32)
    @assert BufferedVectors._grow_by(Int32) > 0

    bv = BufferedVector{Int}([1,2,3], 3)
    shiftleft!(bv, 1)
    @test bv == [2,3]
    @test bv.elements[3] == 3 # the last element is not overwritten

    bv = BufferedVector{Int}([1,2,3], 3)
    shiftleft!(bv, 2)
    @test bv == [3]

    bv = BufferedVector{Int}([1,2,3], 3)
    shiftleft!(bv, 3)
    @test bv == []

    bv = BufferedVector{Int}([1,2,3], 3)
    shiftleft!(bv, 4)
    @test bv == []

    @test_throws ArgumentError shiftleft!(bv, -1)
end
