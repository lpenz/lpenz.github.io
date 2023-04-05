<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">
<title>${title}</title>
% if mathjax:
<script src='https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-AMS-MML_HTMLorMML' async></script>
% endif
<link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css" integrity="sha384-MCw98/SFnGE8fJT3GXwEOngsV7Zt27NXFoaoApmYm81iuXoPkFOJwJ8ERdknLPMO" crossorigin="anonymous">
<link rel="me" href="https://cv.lpenz.org" />
<link rel="me" href="https://twitter.com/lpenz" />
<link rel="me" href="https://www.linkedin.com/in/lpenz" />
<link rel="me" href="https://github.com/lpenz" />
<link rel="me" href="https://stackoverflow.com/cv/lpenz" />
<link rel="me" href="mailto:lpenz@lpenz.org" />
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
% if pandoc_syntax_highlight:
code{white-space: pre-wrap;}
span.smallcaps{font-variant: small-caps;}
span.underline{text-decoration: underline;}
pre > code.sourceCode { white-space: pre; position: relative; }
pre > code.sourceCode > span { display: inline-block; line-height: 1.25; }
pre > code.sourceCode > span:empty { height: 1.2em; }
.sourceCode { overflow: visible; }
code.sourceCode > span { color: inherit; text-decoration: inherit; }
div.sourceCode { margin: 1em 0; }
pre.sourceCode { margin: 0; }
@media screen {
div.sourceCode { overflow: auto; }
}
@media print {
pre > code.sourceCode { white-space: pre-wrap; }
pre > code.sourceCode > span { text-indent: -5em; padding-left: 5em; }
}
pre.numberSource code
  { counter-reset: source-line 0; }
pre.numberSource code > span
  { position: relative; left: -4em; counter-increment: source-line; }
pre.numberSource code > span > a:first-child::before
  { content: counter(source-line);
    position: relative; left: -1em; text-align: right; vertical-align: baseline;
    border: none; display: inline-block;
    -webkit-touch-callout: none; -webkit-user-select: none;
    -khtml-user-select: none; -moz-user-select: none;
    -ms-user-select: none; user-select: none;
    padding: 0 4px; width: 4em;
    color: #aaaaaa;
  }
pre.numberSource { margin-left: 3em; border-left: 1px solid #aaaaaa;  padding-left: 4px; }
div.sourceCode
  {   }
@media screen {
pre > code.sourceCode > span > a:first-child::before { text-decoration: underline; }
}
code span.al { color: #ff0000; font-weight: bold; } /* Alert */
code span.an { color: #60a0b0; font-weight: bold; font-style: italic; } /* Annotation */
code span.at { color: #7d9029; } /* Attribute */
code span.bn { color: #40a070; } /* BaseN */
code span.bu { color: #008000; } /* BuiltIn */
code span.cf { color: #007020; font-weight: bold; } /* ControlFlow */
code span.ch { color: #4070a0; } /* Char */
code span.cn { color: #880000; } /* Constant */
code span.co { color: #60a0b0; font-style: italic; } /* Comment */
code span.cv { color: #60a0b0; font-weight: bold; font-style: italic; } /* CommentVar */
code span.do { color: #ba2121; font-style: italic; } /* Documentation */
code span.dt { color: #902000; } /* DataType */
code span.dv { color: #40a070; } /* DecVal */
code span.er { color: #ff0000; font-weight: bold; } /* Error */
code span.ex { } /* Extension */
code span.fl { color: #40a070; } /* Float */
code span.fu { color: #06287e; } /* Function */
code span.im { color: #008000; font-weight: bold; } /* Import */
code span.in { color: #60a0b0; font-weight: bold; font-style: italic; } /* Information */
code span.kw { color: #007020; font-weight: bold; } /* Keyword */
code span.op { color: #666666; } /* Operator */
code span.ot { color: #007020; } /* Other */
code span.pp { color: #bc7a00; } /* Preprocessor */
code span.sc { color: #4070a0; } /* SpecialChar */
code span.ss { color: #bb6688; } /* SpecialString */
code span.st { color: #4070a0; } /* String */
code span.va { color: #19177c; } /* Variable */
code span.vs { color: #4070a0; } /* VerbatimString */
code span.wa { color: #60a0b0; font-weight: bold; font-style: italic; } /* Warning */
% endif
</style>
<link rel="alternate" type="application/rss+xml" title="Whatsnew feed of Avulsos by Penz" href="http://feeds.feedburner.com/lpenz/avulsos/whatsnew.xml"/>
<link rel="alternate" type="application/rss+xml" title="Articles feed of Avulsos by Penz" href="http://feeds.feedburner.com/lpenz/avulsos/articles.xml"/>
<link rel="icon" type="image/png" href="${top}/media/logo-black.png" />
<meta name="p:domain_verify" content="629162357f7580ddc473183d74c50ef8"/>
<meta name="generator" content="http://txt2tags.sf.net" />
% if disabled:
<script type="text/javascript">
	//<![CDATA[
	alert("This article is disabled. Be warned that it may contain even more errors and inconsistencies than a normal article.");
	//]]>
</script>
% endif
<%block name="htmlheader"/>
</head>

<body>
<%block name="htmlbody"/>
</body>

</html>
<!-- vim: ft=html
-->

