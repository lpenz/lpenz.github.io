<%inherit file="htmlbase.html.mako"/>

<%block name="htmlbody">

<div class="wrapper">

<nav class="navbar navbar-default" role="navigation">
  <div class="container">
    <div class="navbar-header">
      <button type="button" class="navbar-toggle" data-toggle="collapse" data-target=".navbar-collapse">
        <span class="sr-only">Toggle navigation</span>
		<span class="icon-bar"></span>
        <span class="icon-bar"></span>
        <span class="icon-bar"></span>
      </button>
	  <a class="navbar-brand" href="${top}/index.html"><img alt="Avulsos by Penz" src="${top}/media/logo.png" />&nbsp;&nbsp;&nbsp;Avulsos by Penz</a>
    </div>


    <div class="collapse navbar-collapse">
      <ul class="nav navbar-nav nav-tabs navbar-right" role="tablist">
		<li
		% if tab=="home":
		class="active"
		%endif
		><a href="${top}/index.html">Home</a></li>
		<li
		% if tab=="articles":
		class="active"
		%endif
		><a href="${top}/articles/index.html">Articles</a></li>
		<li
		% if tab=="debian":
		class="active"
		%endif
		><a href="${top}/debian/index.html">Debian</a></li>
		<li
		% if tab=="about":
		class="active"
		%endif
		><a href="${top}/about/index.html">About</a></li>
	  </ul>
	</div>

  </div>
</nav>

<div class="container-fluid">
<div class="row">

<div class="col-xs-12 col-md-2">
</div>

<div class="col-xs-12 col-md-8">
	${self.body()}
</div>

<div class="col-xs-4 col-md-2" id="adsense">
</div>

</div> <!--row-->

<div class="row">

<div class="invisible">
  <a href="https://plus.google.com/u/0/101011717297103072571?rel=author">Google Plus authorship information</a>
</div>

<!-- Google Analytics -->
<script>
window.ga=window.ga||function(){(ga.q=ga.q||[]).push(arguments)};ga.l=+new Date;
ga('create', 'UA-4182011-2', 'auto');
ga('set', 'anonymizeIp', true);
ga('send', 'pageview');
</script>
<script async src='https://www.google-analytics.com/analytics.js'></script>
<!-- End Google Analytics -->

<div class="push"> </div>

</div><!--row-->

</div><!--container-->

</div><!--wrapper-->

<div class="footer">
	<div class="center">
		<a rel="license" href="http://creativecommons.org/licenses/by-sa/3.0/"><img alt="Creative Commons License" style="border-width:0; vertical-align:middle;" src="https://licensebuttons.net/l/by-sa/3.0/80x15.png" /></a>
		This work by <a href="mailto:lpenz@lpenz.org">Leandro Lisboa Penz</a> is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-sa/3.0/">Creative Commons Attribution-ShareAlike 3.0 Unported License</a>.
	</div>
</div>

<!-- Other invisible stuff and javascripts: -->

<script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js"></script>
<script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/js/bootstrap.min.js"></script>

</%block>

<!-- vim: ft=html
-->
