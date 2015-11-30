'use strict'
var gulp        = require('gulp')
  , purescript  = require('gulp-purescript')
  , run         = require('gulp-run')
  , runSequence = require('run-sequence')
  , browserify  = require('browserify')
  , plumber     = require('gulp-plumber')
  , buffer      = require('vinyl-buffer')
  , source      = require('vinyl-source-stream')
  ;

function sequence() {
    var args = [].slice.apply(arguments);
    return function() {
        runSequence.apply(null, args);
    };
}

var sources = [
    'src/**/*.purs',
    'bower_components/purescript-*/src/**/*.purs'
];

var foreigns = [
    'src/**/*.js',
    'bower_components/purescript-*/src/**/*.js'
];

var testSources = [
    'test/**/*.purs'
];

var testForeigns = [
    'test/**/*.js'
];

var exampleSources = [
    'examples/src/**/*.purs'
];

var exampleForeigns = [
    'example/src/**/*.js'
];

gulp.task('make', function() {
    return purescript.psc({
        src: sources,
        ffi: foreigns
    });
});

gulp.task('test-make', function() {
    return purescript.psc({
        src: sources.concat(testSources),
        ffi: foreigns.concat(testForeigns)
    });
});

gulp.task('test-bundle', ['test-make'], function() {
    return purescript.pscBundle({
        src: "output/**/*.js",
        main: "Test.Main",
        output: "dist/test.js"
    });
});

gulp.task('test', ['test-bundle'], function() {
    run('phantomjs dist/test.js');
});


gulp.task('example-make', function() {
    return purescript.psc({
        src: sources.concat(exampleSources),
        ffi: foreigns.concat(exampleForeigns)
    });
});

gulp.task('example-bundle', ["example-make"], function() {
    return purescript.pscBundle({
        src: "output/**/*.js",
        main: "Main",
        output: "dist/example.js"
    });
});

gulp.task('example-browserify', ['example-bundle'], function() {
    return browserify({
        entries: ['dist/example.js'],
        paths: ['node_modules']
    })
    .bundle()
        .pipe(plumber())
        .pipe(source('jtable.js'))
        .pipe(buffer())
        .pipe(gulp.dest('examples'));
});

gulp.task("default", sequence("make"));
