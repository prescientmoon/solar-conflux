exports.arraySum = function (v) {
  if (v.length) {
    const [head, ...tail] = v;
    return (head + exports.arraySum(tail)) | 0;
  }

  return 0;
};

exports.arrayFib = (function () {
  var go = function (a) {
    return function (b) {
      return function (v) {
        if (v === 0) {
          return [];
        }
        var c = (a + b) | 0;
        return [c, ...go(c)(a)((v - 1) | 0)];
      };
    };
  };
  return go(1)(1);
})();
