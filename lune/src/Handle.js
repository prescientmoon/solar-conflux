function applyMany(length, f, args) {
  if (length === 0) return f;
  else if (length === 1) return f(args);

  return applyMany(length - 1, f(args.value0), args.value1);
}

/**
 * Collect an arbitrary number of arguments inside an array.
 *
 * @param { number } count The number of arguments to collect.
 * @param { (arguments: any[]) => T } then Callback to give the argument array to.
 */
const collectArguments = (count, then) => {
  if (count === 0) return then([]);

  return (argument) =>
    collectArguments(count - 1, (arguments) => [argument, ...arguments]);
};

exports.curryImpl = (mkTuple, length, f) => {
  if (length <= 1) return f;

  return collectArguments(length, (arguments) => {
    const tuples = arguments.reduceRight(mkTuple);

    return f(tuples);
  });
};

exports.handleImpl = (matchLune, request, casePure, matchers, target) =>
  matchLune(target, {
    pure: casePure,
    request: (length, key, parameters, continuation) => {
      console.log(length);
      if (Reflect.has(matchers, key)) {
        return applyMany(length, matchers[key](continuation), parameters);
      }

      return request((f) =>
        f(key)(parameters)((result) =>
          handleImpl(
            matchLune,
            request,
            casePure,
            matchers,
            continuation(result)
          )
        )
      );
    },
  });
