// module Test.Main

exports.exit = function() {
    if (typeof phantom != "undefined") {
        phantom.exit();
    }
};

exports.inPhantom = function(action) {
    return function() {
        try { action(); }
        catch (e) {
            console.error(e);
            if (typeof phantom != "undefined") {
                phantom.exit();
            }
        }
        finally {
        }
    };
};
