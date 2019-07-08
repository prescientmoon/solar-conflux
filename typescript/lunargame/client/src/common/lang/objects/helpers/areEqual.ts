export const areEqual = <T extends {}>(first: T, last: T) => {
    for (const key of Object.keys(first)) {
        // for ts to shut up
        const typedKey = key as keyof T

        if (first[typedKey] !== last[typedKey]) return false
    }

    return true
}
