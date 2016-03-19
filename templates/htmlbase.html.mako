<!DOCTYPE html>
<html lang="en">

<head>
<meta charset="utf-8"/>
<!--[if IE]><meta http-equiv='X-UA-Compatible' content='IE=edge,chrome=1'><![endif]-->
<meta name="viewport" content="width=device-width, initial-scale=1"/>
<link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css"/>
<!-- HTML5 shim and Respond.js IE8 support of HTML5 elements and media queries -->
<!--[if lt IE 9]>
<script src="https://oss.maxcdn.com/html5shiv/3.7.2/html5shiv.min.js"></script>
<script src="https://oss.maxcdn.com/respond/1.4.2/respond.min.js"></script>
<![endif]-->
<meta name="author" content="Leandro Lisboa Penz"/>
<meta name="description" content="Avulsos by Penz"/>
<style>
html, body { height:100%; background-color: #A6CDBC; }
.navbar { background-color: #A6CDBC; border-bottom: 2px solid #739a89; }
.nav-tabs>li { margin-bottom: -2px; }
.navbar-default .navbar-brand { color: #000000; }
.navbar-default a.navbar-brand:hover { color: #000000; }
.navbar-default .nav-tabs { border-bottom: 0px; }
.navbar-default .navbar-nav>li>a {
	color: #000000;
	font-weight: bold;
	border-top:   2px solid transparent;
	border-right: 2px solid transparent;
	border-left:  2px solid transparent;
}
.navbar-default .nav-tabs>li>a:hover {
	color: #F0CFC2;
	border-bottom: 0px;
	background-color: #739A89;
	border-top:   2px solid #739A89;
	border-right: 2px solid #739A89;
	border-left:  2px solid #739A89;
}
.navbar-default .navbar-nav>.active>a {
	color: #000000;
	background-color: #FFFFFF;
	border-top:   2px solid #739A89;
	border-right: 2px solid #739A89;
	border-left:  2px solid #739A89;
}
.navbar-default .navbar-nav>.active>a:hover {
	color: #000000;
	background-color: #FFFFFF;
	border-top:   2px solid #739A89;
	border-right: 2px solid #739A89;
	border-left:  2px solid #739A89;
	border-bottom:1px solid transparent;
}
.wrapper {
	background-color: #FFFFFF;
	min-height: 100%;
	height: auto !important;
	height: 100%;
	margin: 0 auto -26px;
}
.footer { border-top: 2px solid #739A89; background-color: #A6CDBC; }
.footer>img { vertical-align: text-bottom; }
.footer, .push { height: 26px; }
.footer>div.center { text-align: center; margin: 0 auto; }
h1 { text-align: center; }
h1,h2,h3,h4,h5,h6 { color: #256449; border-bottom: 1px solid #925136; }
div.contents { background-color: #FFFFFF; }
div.center>img { margin: 0 auto; }
</style>
<link rel="alternate" type="application/rss+xml" title="Whatsnew feed of Avulsos by Penz" href="http://feeds.feedburner.com/lpenz/avulsos/whatsnew.xml"/>
<link rel="alternate" type="application/rss+xml" title="Articles feed of Avulsos by Penz" href="http://feeds.feedburner.com/lpenz/avulsos/articles.xml"/>
<link rel="icon" type="image/png" href="${top}/media/logo.png" />
<meta name="p:domain_verify" content="629162357f7580ddc473183d74c50ef8"/>
<meta name="generator" content="http://txt2tags.sf.net" />
% if mathjax:
<script type="text/javascript" src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"></script>
% endif
% if disabled:
<script type="text/javascript">
	//<![CDATA[
	alert("This article is disabled. Be warned that it may contain even more errors and inconsistencies than a normal article.");
	//]]>
</script>
% endif
<title>${title}</title>
<%block name="htmlheader"/>
</head>

<body>
<%block name="htmlbody"/>
</body>

</html>
<!-- vim: ft=html
-->

