// module Data.Json.JSemantic

exports.s2nImpl = function(Just) {
    return function(Nothing) {
        return function(s) {
            var n = s * 1;
            if (isNaN(n)) {
                return Nothing;
            }
            else {
                return Just(n);
            }
        };
    };
};

exports.toString = function(d) {
    return d.toString();
};
