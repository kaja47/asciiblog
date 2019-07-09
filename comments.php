<?php

error_reporting(0);

$commentsModeration = '{comments.moderation}' === 'true';

function escapeHtml($s) {
  return htmlspecialchars($s, ENT_QUOTES, 'UTF-8');
}

function escapeHtmlAttr($s) {
  if (strpos($s, '`') !== false && strpbrk($s, ' <>"\'') === false) { $s .= ' '; }
  return htmlspecialchars($s, ENT_QUOTES, 'UTF-8');
}

class CommentSection {
  private $commentsFile      = '.comments/comments';
  private $articlesFile      = '.comments/articles';
  private $ipBlacklistFile   = '.comments/ip-blacklist';
  private $termBlacklistFile = '.comments/term-blacklist';

  function isSpam($comment) {
    if (file_exists($this->ipBlacklistFile)) {
      $ips = array_map('trim', file($this->ipBlacklistFile));
      foreach ($ips as $ip) {
        if (substr($comment->ip, 0, strlen($ip)) === $ip) {
          return true;
        }
      }
    }

    if (file_exists($this->termBlacklistFile)) {
      $terms = array_map('trim', file($this->termBlacklistFile));
      foreach ($terms as $term) {
        if (strpos($comment->text, $term) !== false) {
          return true;
        }
      }
    }

    return false;
  }

  function addComment($comment) {
    if ($comment->slug === null) throw new \Exception("missing slug");
    if ($comment->text === '') throw new \Exception("Text field is required.");
    if (strlen($comment->name) > 60) throw new \Exception("Name is too long.");
    if (strlen($comment->mail) > 60) throw new \Exception("Mail is too long.");
    if (strlen($comment->web)  > 60) throw new \Exception("Web is too long.");
    if (strlen($comment->text) > 2000) throw new \Exception("Text is too long.");
    if ($comment->name === '') { $comment->name = 'Anonymous'; }

    if ($this->isSpam($comment)) {
      throw new \Exception("Spam detected.");
    }

    if (!$comment->approved) { // auto-approve
      $shouldAutoapprove = false;
      foreach ($this->getComments(null)->comments as $c) {
        if ($c->approved && $c->ip === $comment->ip) {
          $shouldAutoapprove = true;
          break;
        }
      }
      $comment->approved = $shouldAutoapprove;
    }

    if (isset($comment->replyTo)) { // check if valid reply
      $validReply = false;
      foreach ($this->getComments($comment->slug)->comments as $c) {
        if ($c->id == $replyTo) {
          $validReply = true;
          break;
        }
      }

      if (!$validReply || $comment->replyTo == $comment->id) {
        throw new \Exception("Replying to invalid comment.");
      }
    }

    file_put_contents($this->commentsFile, json_encode($comment)."\n", FILE_APPEND | LOCK_EX);
    return $comment;
  }

  function getComments($slug = null, $flat = true, $allComments = false) {
    $articles = []; // [slug => title]
    foreach (file($this->articlesFile) as $l) {
      list($s, $t) = explode(" ", trim($l), 2);
      $articles[$s] = $t;
    }

    if ($slug !== null) {
      if (!isset($articles[$slug])) throw new \Exception("invalid slug");
    }

    $comments = array_map(function ($l) { return json_decode(trim($l)); }, file($this->commentsFile));
    if ($slug !== null) {
      $comments = array_filter($comments, function ($c) use ($slug) { return $c->slug === $slug; });
    }

    $cs = array();
    $i = 0;
    foreach ($comments as $c) {
      if ($allComments || !isset($c->approved) || $c->approved) {
        $cs[isset($c->id) ? $c->id : $i++] = $c;
      }
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

    return (object) array(
      'title' => $slug !== null ? $articles[$slug] : '{comments.commentsTo}',
      'slug' => $slug,
      'comments' => $cs,
    );
  }

}

function mkText($txt) {
  return str_replace("\n", "<br>", $txt);
}


try {

  $commentSection = new CommentSection;
  $requestUrl = $_SERVER['REQUEST_URI'];
  $slug = (string) $_GET['u'];
  $slug = $slug === "" ? null : $slug;
  $toBeApproved = isset($_GET['app']);

  if (isset($_POST['text'])) {
    $comment = (object) array(
      'slug' => $slug,
      'date' => date("Y-m-d H:i:s", time()),
      'approved' => !$commentsModeration,
      'id'   => mt_rand(),
      'name' => (string) $_POST['name'],
      'mail' => (string) $_POST['mail'],
      'web'  => (string) $_POST['web'],
      'ip'   => (string) $_SERVER['REMOTE_ADDR'],
      'text' => (string) $_POST['text'],
    );

    if ($_POST['replyTo']) {
      $comment->replyTo = (int) $_POST['replyTo'];
    }

    $savedComment = $commentSection->addComment($comment);
    setcookie('user', json_encode(array($comment->name, $comment->mail, $comment->web)), time()+3600*24*30);
    header('Location: '.$requestUrl.(!$savedComment->approved ? "&app" : ""));
    exit;

  } elseif (isset($_GET['rss'])) {
    $allComments = isset($_GET['all']); // get all comments including not yet approved
    $block = $slug ? $commentSection->getComments($slug, true, false) : $commentSection->getComments(null, true, $allComments);
    usort($block->comments, function ($a, $b) { return strcmp($b->date, $a->date); });
    $block->comments = array_slice($block->comments, 0, 20);

    $rss = new \SimpleXMLElement('<rss version="2.0"></rss>');
    $rss->channel->title = $block->title;
    $rss->channel->link  = "{comments.baseUrl}/comments.php?u=".escapeHtmlAttr($block->slug);

    foreach ($block->comments as $c) {
      $item = $rss->channel->addChild("item");
      $item->title = $block->title." - ".$c->name;
      $item->description = escapeHtml($c->text);
      $item->guid  = "{comments.baseUrl}/comments.php?url=".escapeHtmlAttr(isset($c->slug) ? $c->slug : $block->slug)."#".$c->id;
      $item->guid["isPermalink"] = "true";
      $item->pubDate = date(DATE_RSS, strtotime($c->date));
    }

    header("Content-Type: application/xml; charset=UTF-8");
    echo $rss->asXML();
    exit;

  } else {
    if ($slug === null) throw new \Exception("missing slug");
    $block = $commentSection->getComments($slug, false, false);
    @list($name, $mail, $web) = isset($_COOKIE['user']) ? array_map('strval', json_decode((string)$_COOKIE['user'])) : array('', '', '');

    echo '{comments.prebody}';
    echo "{comments.commentsTo} <h2><a href='", escapeHtmlAttr($block->slug) , "'>", escapeHtml($block->title), "</a></h2><br><br>";

    echo '
<style>
textarea { width: 100%; height: 6em; }
input { padding: 1px 2px; margin: 0em; border:1px solid gray; width: 7em; }
form div { float: left; margin-left: 0.5em }
</style>

<form action="'.escapeHtmlAttr($requestUrl).'" method=post>
  <legend>{comments.text}</legend>
  <textarea name=text required></textarea>
  <br>

  <div>
    <label for=name>{comments.name}</label>
    <input type=text maxlength=60 name=name id=name value="'.escapeHtmlAttr($name).'">
  </div>

  <div>
    <label for=mail>{comments.mail}</label>
    <input type=email maxlength=60 name=mail id=mail value="'.escapeHtmlAttr($mail).'">
  </div>

  <div>
    <label for=web>{comments.web}</label>
    <input type=url maxlength=60 name=web id=web value="'.escapeHtmlAttr($web).'">
  </div>

  <div>
    <input type=submit name=send value="{comments.submit}">
  </div>
  <br clear=all>

</form>
<br>';

    if ($toBeApproved) {
      echo "<p>{comments.approve}</p>";
    }

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
