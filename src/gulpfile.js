'use strict';

var gulp     = require('gulp');
var sass     = require('gulp-sass');
var prefixer = require('gulp-autoprefixer');
var elm      = require('gulp-elm');
var rename   = require('gulp-rename');

// SASS

gulp.task('sass', function () {
    gulp.src('scss/**/*.scss')
        .pipe(sass({outputStyle: 'compact'}).on('error', sass.logError))
        .pipe(prefixer().on('error', function(e) { console.log(e.message); }))
        .pipe(gulp.dest('../build/css'));
});

// ELM

gulp.task('elm-init', elm.init);

gulp.task('elm', function(){
    gulp.src('app/Main.elm')
        .pipe(elm()).on('error', function(e) { console.log(e.message); })
        .pipe(rename('app.js'))
        .pipe(gulp.dest('../build/js'));
});

// RUN ALL

gulp.task('default', function () {
    gulp.watch('scss/**/*.scss', ['sass']);
    gulp.watch('app/*.elm', ['elm']);
});
