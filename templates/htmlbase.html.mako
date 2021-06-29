<!DOCTYPE html>
<html lang="en">
<head>
<link rel="me" href="https://cv.lpenz.org" />
<link rel="me" href="https://twitter.com/lpenz" />
<link rel="me" href="https://www.linkedin.com/in/lpenz" />
<link rel="me" href="https://github.com/lpenz" />
<link rel="me" href="https://stackoverflow.com/cv/lpenz" />
<link rel="me" href="mailto:lpenz@lpenz.org" />
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">
<link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css" integrity="sha384-MCw98/SFnGE8fJT3GXwEOngsV7Zt27NXFoaoApmYm81iuXoPkFOJwJ8ERdknLPMO" crossorigin="anonymous">
<meta name="author" content="Leandro Lisboa Penz"/>
<meta name="description" content="Avulsos by Penz"/>
<style>
html { /* get footer on bottom: */
    position: relative;
    min-height: 100%;
}
body {
    margin-bottom: 30px; /* Margin bottom by footer height */
}
.footer {
    position: absolute;
    bottom: 0;
    width: 100%;
    height: 30px; /* Set the fixed height of the footer here */
    line-height: 30px; /* Vertically center the text there */
    /* background-color: #f5f5f5; */
}
nav.navbar {
    margin-bottom: 2em; /* space between navbar and contents */
}
pre {
    background-color: #f1f1f1;
    border-left: 4px solid #d9d9d9;
    padding: 15px;
}
#title {
    text-align: center;
    margin-bottom: 1em;
    font-size: 2.5rem;
}
h1, h2, h3, h4, h5, h6 { margin-top: 1em; margin-bottom: 1em; }
h1 { font-size: 1.6rem; }
h2 { font-size: 1.4rem; }
h3, h4, h5, h6 { font-size: 1.2rem; }
td > ul { margin-bottom: 0px; }
em {
    color: indigo;
}
div#main {
    padding-bottom: 1em;
}
div.center > img { margin: 0 auto; } /* center images by default */
div#toc {
    background: #f9f9f9 none repeat scroll 0 0;
    border: 1px solid #aaa;
    display: table;
}
div#toc .toctitle {
    font-weight: 700;
    text-align: center;
    margin-top: 1em;
}
div#toc ul {
    padding-right: 1em;
}
div#toc li, div#toc ul, div#toc ul li {
    list-style: outside none none !important;
}
</style>
<link rel="alternate" type="application/rss+xml" title="Whatsnew feed of Avulsos by Penz" href="http://feeds.feedburner.com/lpenz/avulsos/whatsnew.xml"/>
<link rel="alternate" type="application/rss+xml" title="Articles feed of Avulsos by Penz" href="http://feeds.feedburner.com/lpenz/avulsos/articles.xml"/>
<link rel="icon" type="image/png" href="${top}/media/logo-black.png" />
<meta name="p:domain_verify" content="629162357f7580ddc473183d74c50ef8"/>
<meta name="generator" content="http://txt2tags.sf.net" />
% if mathjax:
<script src='https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-AMS-MML_HTMLorMML' async></script>
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

