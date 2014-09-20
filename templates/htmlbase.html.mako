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
<link rel="stylesheet" type="text/css" href="${top}/style.css" />
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

