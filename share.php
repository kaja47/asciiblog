<?php

$links = [
  "f" => "https://www.facebook.com/sharer/sharer.php?u=",
  "t" => "https://twitter.com/intent/tweet?url=",
  "l" => "https://www.linkedin.com/shareArticle?mini=true&url=",
  "u" => "https://www.tumblr.com/share/link?url=",
];

foreach ($links as $param => $link) {
  if (isset($_GET[$param])) {
    $slug = (string) $_GET[$param];
    header("Location: ${link}{slug}");
    return;
  }
}

header("HTTP/1.0 404 Not Found");
