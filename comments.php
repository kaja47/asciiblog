<?php

error_reporting(0);

function escapeHtml($s) {
	return htmlspecialchars($s, ENT_QUOTES, 'UTF-8');
}

function escapeHtmlAttr($s) {
  if (strpos($s, '`') !== false && strpbrk($s, ' <>"\'') === false) { $s .= ' '; }
  return htmlspecialchars($s, ENT_QUOTES, 'UTF-8');
}

class CommentSection {
	private $baseDir = ".comments";
	private $pathPattern;

	function __construct($pathPattern) {
		list($this->pathPattern) = func_get_args();
	}

	private function getFile($path) {
		 return $this->baseDir.'/'.str_replace("/", "_", $path);
	}

	function addComment($path, $comment) {
		$this->openFile($path);
		file_put_contents($this->getFile($path), json_encode($comment)."\n", FILE_APPEND | LOCK_EX);
	}

	function getComments($path) {
		$this->openFile($path);
		$lines = array_map('trim', file($this->getFile($path)));
		$block = json_decode($lines[0]);
		$block->comments = array_map('json_decode', array_slice($lines, 1));
		return $block;
	}

	private function openFile($path) {
		if ($path[0] == '/' || strpos($path, '..') !== false) {
			throw new \Exception("invalid path");
		}
		if ($this->urlPattern && !preg_match($this->urlPattern, $path)) {
			throw new \Exception("invalid path");
		}
		$f = $this->getFile($path);
		if (!file_exists($f)) {
			if (!file_exists($this->baseDir)) {
				mkdir($this->baseDir);
			}
			$html = @file_get_contents($path);
			if ($html === false) {
				throw new \Exception("page '$path' cannot be found");
			}
			$dom = new \DOMDocument('1.0', 'UTF-8');
			$html = mb_convert_encoding($html, 'HTML-ENTITIES', "UTF-8");
			$ok = @ $dom->loadHTML($html);
			if ($ok === false) {
				throw new \Exception("page '$path' cannot be found");
			}
			$xpath = new \DOMXPath($dom);
			$title = $xpath->query('//h2')->item(0)->nodeValue;
			$line = json_encode(array('path' => $path, 'title' => $title))."\n";
			file_put_contents($f, $line);
		}
	}
}


try {

$commentSection = new CommentSection('~.*\.html~');
$requestUrl = $_SERVER['REQUEST_URI'];
$url = (string) $_GET['url'];

if (isset($_POST['text'])) {
	$comment = (object) array(
		'name' => (string) $_POST['name'],
		'mail' => (string) $_POST['mail'],
		'web'  => (string) $_POST['web'],
		'text' => (string) $_POST['text'],
		'date' => time(),
		'ip'   => (string) $_SERVER['REMOTE_ADDR'],
	);

	if ($comment->text === '') throw new \Exception("Text field is required.");
	if (strlen($comment->name) > 60) throw new \Exception("Name is too long.");
	if (strlen($comment->mail) > 60) throw new \Exception("Mail is too long.");
	if (strlen($comment->web)  > 60) throw new \Exception("Web is too long.");
	if (strlen($comment->text) > 2000) throw new \Exception("Text is too long.");
	if ($comment->name === '') { $comment->name = 'Anonymous'; }

	$commentSection->addComment($url, $comment);
	header('Location: '.$requestUrl);
	exit;

} elseif (isset($_GET['rss'])) {
	$block = $commentSection->getComments($url);

	$rss = new \SimpleXMLElement('<rss version="2.0"></rss>');
	$rss->channel->title = $block->title;
	//$rss->channel->link  = $block->url;

	foreach ($block->comments as $c) {
		$item = $rss->channel->addChild("item");
		$item->title = $block->title.' - '.$c->name;
		$item->description = $c->text;
		//$item->guid  = $block->url."#".$c->date;
		$item->pubDate = date(DATE_RSS, $c->date);
	}

	header("Content-Type: application/xml; charset=UTF-8");
	echo $rss->asXML();
	exit;

} else {
	$block = $commentSection->getComments($url);

	echo '{comments.prebody}';
	echo "{comments.commentsTo} <h2><a href='", escapeHtmlAttr($block->path) , "'>", escapeHtml($block->title), "</a></h2><br/></br/>";

	foreach ($block->comments as $c) {
		if ($c->web) {
			echo "<a href='", escapeHtmlAttr($c->web), "'><i>", escapeHtml($c->name), "</i></a> ";
		} else {
			echo                                         "<i>", escapeHtml($c->name), "</i> ";
		}
		echo "<span style='color: gray;'>(", date("Y-m-d H:i", $c->date), ")</span><br/>";
		echo escapeHtml($c->text);
		echo "<br/><br/>";
	}

	echo '
<style>
textarea { width: 100%; height: 6em; }
input { padding: 1px 2px; margin: 0em; border:1px solid gray; width: 7em; }
form div { float: left; margin-left: 0.5em }
</style>

<form action="'.escapeHtmlAttr($requestUrl).'" method="post">
	<legend>{comments.text}</legend>
	<textarea name="text" required></textarea>
	<br/>

	<div>
		<label for="name">{comments.name}</label>
		<input type="text" maxlength="60" name="name" id="name">
	</div>

	<div>
		<label for="mail">{comments.mail}</label>
		<input type="email" maxlength="60" name="mail" id="mail">
	</div>

	<div>
		<label for="web">{comments.web}</label>
		<input type="url" maxlength="60" name="web" id="web">
	</div>

	<div>
		<input type="submit" name="send" value="{comments.submit}">
	</div>
	<br clear=all />

</form>';

	echo '{comments.postbody}';
	exit;
}

} catch (Exception $e) {
	echo $e->getMessage();
	exit;
}
