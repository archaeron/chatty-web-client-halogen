'use strict'

var gulp		= require('gulp')
  , purescript  = require('gulp-purescript')
  , browserify  = require('gulp-browserify')
  , run		 = require('gulp-run')
  , runSequence = require('run-sequence')
  , jsValidate  = require('gulp-jsvalidate')
  ;

var paths = {
	src: {
		src: [
			'src/**.purs',
			'bower_components/purescript-halogen/purescript-halogen-bootstrap/src/**/*.purs'
		],
		dest: 'js',
		options: {
			main: 'Example.Ace',
			modules: ['Example.Ace']
		}
	},
	bowerSrc: [
		'bower_components/purescript-*/src/**/*.purs'
	],
};

function compile (compiler, src, opts) {
	var psc = compiler(opts);
	psc.on('error', function(e) {
		console.error(e.message);
		psc.end();
	});
	return gulp.src(src.concat(paths.bowerSrc))
		.pipe(psc)
		.pipe(jsValidate());
};

gulp.task('make', function() {
	return compile(purescript.psc, paths.src.src, paths.src.options)
		.pipe(browserify({}))
		.pipe(gulp.dest(paths.src.dest));
});

gulp.task('default', function(cb) {
	runSequence('make', cb);
});
