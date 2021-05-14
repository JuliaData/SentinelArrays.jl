using FGenerators: @fgenerator, @yield
using SplittablesBase: SplittablesBase

@fgenerator(A::ChainedVector) do
    for array in A.arrays
        for x in array
            @yield x
        end
    end
end

function SplittablesBase.halve(A::ChainedVector)
    chunk = searchsortedfirst(A.inds, length(A) ÷ 2)
    left = @view A.arrays[1:chunk]
    right = @view A.arrays[chunk+1:end]
    return (Iterators.flatten(left), Iterators.flatten(right))
end
