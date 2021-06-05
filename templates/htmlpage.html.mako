<%inherit file="htmlbase.html.mako"/>

<%block name="htmlbody">

<nav class="navbar navbar-expand-md navbar-dark bg-dark">
    <div class="container">
    <a class="navbar-brand" href="${top}/index.html"><img alt="Avulsos by Penz" src="${top}/media/logo-white.png" width="20" height="20" class="d-inline-block align-middle" />&nbsp;&nbsp;&nbsp;Avulsos by Penz</a>
    <button class="navbar-toggler" type="button" data-toggle="collapse" data-target="#navbarSupportedContent" aria-controls="navbarSupportedContent" aria-expanded="false" aria-label="Toggle navigation">
        <span class="navbar-toggler-icon"></span>
    </button>
    <div class="collapse navbar-collapse auto" id="navbarSupportedContent">
        <ul class="navbar-nav ml-auto">
		<li
		% if tab=="home":
		class="nav-item active"
        % else:
		class="nav-item"
		% endif
		><a class="nav-link" href="${top}/index.html">Home</a></li>
		<li
		% if tab=="articles":
		class="nav-item active"
        % else:
		class="nav-item"
		% endif
		><a class="nav-link" href="${top}/articles/index.html">Articles</a></li>
		<li
		% if tab=="debian":
		class="nav-item active"
        % else:
		class="nav-item"
		% endif
		><a class="nav-link" href="${top}/debian/index.html">Debian</a></li>
		<li
		% if tab=="about":
		class="nav-item active"
        % else:
		class="nav-item"
		% endif
		><a class="nav-link" href="${top}/about/index.html">About</a></li>
    </div>
    </div>
</nav>

<div class="container" id="main">

<h1 id="title">${title}</h1>

${self.body()}

<!-- Google Analytics -->
<script>
window.ga=window.ga||function(){(ga.q=ga.q||[]).push(arguments)};ga.l=+new Date;
ga('create', 'UA-4182011-2', 'auto');
ga('set', 'anonymizeIp', true);
ga('send', 'pageview');
</script>
<script async src='https://www.google-analytics.com/analytics.js'></script>
<!-- End Google Analytics -->

</div><!--container-->

<div class="footer bg-dark text-light">
	<div class="container">
		<a rel="license" href="http://creativecommons.org/licenses/by-sa/3.0/"><img alt="Creative Commons License" style="border-width:0; vertical-align:middle;" src="https://licensebuttons.net/l/by-sa/3.0/80x15.png" /></a>
		This work by <a href="mailto:lpenz@lpenz.org">Leandro Lisboa Penz</a> is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-sa/3.0/">Creative Commons Attribution-ShareAlike 3.0 Unported License</a>.
	</div>
</div>

<!-- Other invisible stuff and javascripts: -->

<script src="https://code.jquery.com/jquery-3.3.1.slim.min.js" integrity="sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo" crossorigin="anonymous"></script>
<script src="https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.3/umd/popper.min.js" integrity="sha384-ZMP7rVo3mIykV+2+9J3UJ46jBk0WLaUAdn689aCwoqbBJiSnjAK/l8WvCWPIPm49" crossorigin="anonymous"></script>
<script src="https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/js/bootstrap.min.js" integrity="sha384-ChfqqxuZUCnJSK3+MXmPNIyE6ZbWh2IMqE241rYiqJxyMiZ6OW/JmZQ5stwEULTy" crossorigin="anonymous"></script>

</%block>

<!-- vim: ft=html
-->
