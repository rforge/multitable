
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='http://r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>




<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<div id="wrap"><img id="frontphoto" alt="" src="multitablelogo.png" style="padding-top: 8px;" />	

<h2>General multiple-table data management in R</h2>
	
<p>Our objective is to develop classes of objects that (1) make handling multiple-table data sets easier and (2) seamlessly integrate with existing R plotting and model-fitting functions; our philosophy is to keep data management and data analysis separate.
<!-- end of project description -->

<p> Many ecologists and other scientists face a particular kind of data management issue, arising from data sets that cannot be expressed as a single table, matrix, or array.  The prototypical example of this issue, called the fourth-corner problem (Legendre et al. 1997), comes from trait-based studies of ecological communities in which the ecologist must manage three tables:  a sites-by-species table of community data, a sites-by-environmental variables table, and a species-by-traits table.  Such a data set cannot be expressed in a single table (e.g. an object of class data frame or array) without either (1) summarizing or (2) repeating chunks of data; hence, the problem is inherently multi-tabular.</p>

<p> The multitable project involves the development of R tools supporting the analysis of such multiple-table data.  The specific objective of the multitable project is to develop new classes of objects that make handling multiple-table data sets easier, and seamlessly integrate with existing R model fitting functions.  The framework being developed is based on a separation of data management and data analysis.  Once multiple-table data are organized into a special multiple-table R object (i.e. the data management step) they can be passed to plotting and model fitting functions (i.e. the data analysis step), just as we would pass data frames to such functions in R.  Having separated analysis from data management, analyzing multiple tables becomes conceptually equivalent to analyzing single tables.</p>

<p> Major features: (1) subscripting of multiple tables simultanenously, (2) coercion of multiple tables into a single data frame for use in standard R model fitting and plotting functions.</p>

<p> <strong>NOW ON CRAN!!</strong> <a href="http://cran.r-project.org/web/packages/multitable/"><strong>here</strong></a>. </p>

<p> The development version is <a href = "http://r-forge.r-project.org/R/?group_id=1171"><strong>here</strong></a>. </p>

<p> The <strong>project summary page</strong> can be found <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. </p>

<p> A presentation on the multitable package at the Ecological Society of America can be found <a href="https://r-forge.r-project.org/scm/viewvc.php/*checkout*/www/ESA2011.pdf?root=multitable"><strong>here</strong></a>. </p>

<p> Check out the <strong>new</strong> package vignette <a href="https://r-forge.r-project.org/scm/viewvc.php/*checkout*/www/multitableIntro.pdf?root=multitable"><strong>here</strong></a>. </p>

<p> <a href = "staticdocs/index.html"><strong>mip</strong></a>. </p>

</body>
</html>
