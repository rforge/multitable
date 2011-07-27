
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

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="http://r-forge.r-project.org/"><img src="http://<?php echo $themeroot; ?>/images/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } ?>

<!-- end of project description -->

<p> Many ecologists and other scientists face a particular kind of data management issue, arising from data sets that cannot be expressed as a single table, matrix, or array.  The prototypical example of this issue, called the fourth-corner problem (Legendre et al. 1997), comes from trait-based studies of ecological communities in which the ecologist must manage three tables:  a sites-by-species table of community data, a sites-by-environmental variables table, and a species-by-traits table.  Such a data set cannot be expressed in a single table (e.g. an object of class data frame or array) without either (1) summarizing or (2) repeating chunks of data; hence, the problem is inherently multi-tabular.  

The multitable project involves the development of R tools supporting the analysis of such multiple-table data.  The specific objective of the multitable project is to develop new classes of objects that make handling multiple-table data sets easier, and seamlessly integrate with existing R model fitting functions.  The framework being developed is based on a separation of data management and data analysis.  Once multiple-table data are organized into a special multiple-table R object (i.e. the data management step) they can be passed to plotting and model fitting functions (i.e. the data analysis step), just as we would pass data frames to such functions in R.  Having separated analysis from data management, analyzing multiple tables becomes conceptually equivalent to analyzing single tables.</p>

<p> The <strong>project summary page</strong> you can find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. </p>

</body>
</html>
