-- Compatibility fixes because lovejs sucks...

-- lovejs doesn't like when the arguments are given in reverse
local oldrandom = math.random
math.random = function(low, high)
    if low <= high then
        return oldrandom(low, high)
    else
        return oldrandom(high, low)
    end
end
