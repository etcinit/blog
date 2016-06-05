var gulp = require('gulp');
var postcss = require('gulp-postcss');
var $ = require('gulp-load-plugins')();
var autoprefixer = require('autoprefixer');
var cssnano = require('cssnano');

var sassPaths = [
  'bower_components/foundation-sites/scss',
  'bower_components/motion-ui/src',
  'bower_components/font-awesome/scss',
  'scss',
];

gulp.task('sass', function() {
  var processors = [
    autoprefixer({browsers: ['last 2 version', 'ie >= 9']}),
    cssnano(),
  ];

  return gulp.src('scss/app.scss')
    .pipe($.sass({
      includePaths: sassPaths
    })
    .on('error', $.sass.logError))
    .pipe(postcss(processors))
    .pipe(gulp.dest('css'));
});

gulp.task('default', ['sass'], function() {
  gulp.watch(['scss/**/*.scss'], ['sass']);
});
