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
	private $baseDir    = '.comments';
	private $pathRegex  = '~.*\.html~';
	private $globalPath = 'global.rss.html';

	function addComment($path, $comment) {
		$this->append($this->openFileFor($path), $comment);
		$this->append($this->openFileGlobal(),   $comment);
	}

	function getComments($path, $flat = false) {
		$lines = array_map('json_decode', array_map('trim', file($this->openFileFor($path))));
		$block = $lines[0];
		$cs = array();
		foreach (array_slice($lines, 1) as $c) {
			$cs[$c->id] = $c;
		}
		if (!$flat) {
			foreach (array_reverse($cs) as $c) {
				if (isset($c->replyTo) && $cs[$c->replyTo]) {
					if (!isset($cs[$c->replyTo]->replies)) $cs[$c->replyTo]->replies = array();
					array_unshift($cs[$c->replyTo]->replies, $c);
					unset($cs[$c->id]);
				}
			}
		}
		$block->comments = $cs;
		return $block;
	}

	function getCommentsGlobal() {
		$block = $this->getComments($this->globalPath, true);
		$block->title = '{comments.commentsTo}';
		return $block;
	}

	private function openFileFor($path) {
		return $this->openFile($path, true);
	}

	private function openFileGlobal() {
		return $this->openFile($this->globalPath, false);
	}

	private function getFile($path) {
		 return $this->baseDir.'/'.str_replace("/", "_", $path);
	}

	private function checkPath($path) {
		if ($path[0] === '/' || strpos($path, '..') !== false || !preg_match($this->pathRegex, $path))
			throw new \Exception("invalid path");
	}

	private function openFile($path, $fetchTitle) {
		$this->checkPath($path);
		$f = $this->getFile($path);
		if (!file_exists($f)) {
			if (!file_exists($this->baseDir)) {
				mkdir($this->baseDir);
			}
			$header = array('path' => $path, 'title' => $fetchTitle ? $this->getTitle($path) : null);
			$this->append($f, $header);
		}
		return $f;
	}

	private function getTitle($path) {
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
		return $xpath->query('//h2')->item(0)->nodeValue;
	}

	private function append($file, $data) {
		file_put_contents($file, json_encode($data)."\n", FILE_APPEND | LOCK_EX);
	}
}

function mkText($txt) {
	return str_replace("\n", "<br/>", $txt);
}


try {

$commentSection = new CommentSection;
$requestUrl = $_SERVER['REQUEST_URI'];
$url = (string) $_GET['url'];

if (isset($_POST['text'])) {
	$comment = (object) array(
		'id'   => mt_rand(),
		'name' => (string) $_POST['name'],
		'mail' => (string) $_POST['mail'],
		'web'  => (string) $_POST['web'],
		'text' => (string) $_POST['text'],
		'date' => date("Y-m-d H:i:s", time()),
		'ip'   => (string) $_SERVER['REMOTE_ADDR'],
	);

	if ($comment->text === '') throw new \Exception("Text field is required.");
	if (strlen($comment->name) > 60) throw new \Exception("Name is too long.");
	if (strlen($comment->mail) > 60) throw new \Exception("Mail is too long.");
	if (strlen($comment->web)  > 60) throw new \Exception("Web is too long.");
	if (strlen($comment->text) > 2000) throw new \Exception("Text is too long.");
	if ($comment->name === '') { $comment->name = 'Anonymous'; }

	$commentSection->addComment($url, $comment);
	setcookie('user', json_encode(array($comment->name, $comment->mail, $comment->web)), time()+3600*24*30);
	header('Location: '.$requestUrl);
	exit;

} elseif (isset($_GET['rss'])) {
	$block = $url ? $commentSection->getComments($url) : $commentSection->getCommentsGlobal();

	$rss = new \SimpleXMLElement('<rss version="2.0"></rss>');
	$rss->channel->title = $block->title;
	$rss->channel->link  = "{comments.baseUrl}/comments.php?url=".escapeHtmlAttr($block->path);

	foreach ($block->comments as $c) {
		$item = $rss->channel->addChild("item");
		$item->title = "$block->title - $c->name";
		$item->description = $c->text;
		$item->guid  = "{comments.baseUrl}/comments.php?url=".escapeHtmlAttr($block->path)."#".$c->id;
		$item->guid["isPermalink"] = "true";
		$item->pubDate = date(DATE_RSS, strtotime($c->date));
	}

	header("Content-Type: application/xml; charset=UTF-8");
	echo $rss->asXML();
	exit;

} else {
	$block = $commentSection->getComments($url);
	@list($name, $mail, $web) = isset($_COOKIE['user']) ? array_map('strval', json_decode((string)$_COOKIE['user'])) : array('', '', '');

	echo '{comments.prebody}';
	echo "{comments.commentsTo} <h2><a href='", escapeHtmlAttr($block->path) , "'>", escapeHtml($block->title), "</a></h2><br/></br/>";

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
		<input type="text" maxlength="60" name="name" id="name" value="'.escapeHtmlAttr($name).'">
	</div>

	<div>
		<label for="mail">{comments.mail}</label>
		<input type="email" maxlength="60" name="mail" id="mail" value="'.escapeHtmlAttr($mail).'">
	</div>

	<div>
		<label for="web">{comments.web}</label>
		<input type="url" maxlength="60" name="web" id="web" value="'.escapeHtmlAttr($web).'">
	</div>

	<div>
		<input type="submit" name="send" value="{comments.submit}">
	</div>
	<br clear=all />

</form>
<br/><br/>
';

	function printComments($comments) {
		foreach ($comments as $c) {
			if ($c->web) {
				echo "<a href='", escapeHtmlAttr($c->web), "'><i id='{$c->id}'>", escapeHtml($c->name), "</i></a> ";
			} else {
				echo                                         "<i id='{$c->id}'>", escapeHtml($c->name), "</i> ";
			}
			echo "<span style='color: gray;'>(", date("Y-m-d H:i", strtotime($c->date)), ")</span><br/>";
			echo mkText(escapeHtml($c->text));
			echo "<br/><br/>";

			if (isset($c->replies)) {
				echo "<div style='margin-left:2em;'>";
				printComments($c->replies);
				echo "</div>";
			}
		}
	}

	printComments($block->comments);

	echo '{comments.postbody}';
	exit;
}

} catch (Exception $e) {
	echo $e->getMessage();
	exit;
}
