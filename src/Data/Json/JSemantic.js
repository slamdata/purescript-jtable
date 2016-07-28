"use strict";

exports.s2nImpl = function (just) {
  return function (nothing) {
    return function (s) {
      var n = s * 1;
      return isNaN(n) ? nothing : just(n);
    };
  };
};
