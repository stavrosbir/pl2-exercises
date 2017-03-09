<?php
if ("${_SERVER['QUERY_STRING']}" == "")
  $self = "${_SERVER['PHP_SELF']}";
else
  $self = "${_SERVER['PHP_SELF']}?${_SERVER['QUERY_STRING']}";

session_start();

if (!isset($_SESSION['number']) || isset($_POST['reset'])) {
  $_SESSION['number'] = rand(100000000, 999999999);
}
?>

<!DOCTYPE html PUBLIC
          "-//W3C//DTD XHTML 1.0 Transitional//EN"
          "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
<title>Funge game!</title>
<style type="text/css">
<!--
body,td,th {
  font-family: Verdana, Arial, Helvetica, sans-serif;
  font-size: x-large;
  color: #CCCCCC;
}

body {
  background-color: #333399;
}

.title {
  font-family: "Courier New", Courier, monospace;
  font-weight: bold;
  font-size: 48px;
  color: #00FF66;
}

.question {color: #FFCC33}
.number {color: #FFFF33}
.md5sum {color: #FFCCFF}
.emph {color: #99ee99}
.alert {color: #ee77aa}

.right {
  color: #33FF66;
  font-weight: bold;
}

.wrong {
  color: #FF3366;
  font-weight: bold;
}

a:link {
  color: #CCFFFF;
}

a:visited {
  color: #CCFFFF;
}

textarea {
  background-color: #eeee66;
  color: #333399;
}

textarea.wide {
  font-family: monospace;
  font-size: x-large;
  color: #333333;
  border: 1px solid black;
  padding: 8px;
}
-->
</style>
</head>
<body>
<h1 class="title">Help!</h1>
<p>I need to find a <a href="http://catseye.tc/view/befunge-93/doc/Befunge-93.markdown">Befunge-93</a> program that outputs the number <span class="question"><?php echo $_SESSION['number'] ?></span>.</p>
<p>But I also need the program's total area to be as small as possible.<br />
(Don't worry, it doesn't have to be optimal...)</p>
<p>Oh, one more thing: The commands
  <code class="emph">0-9</code>,
  <code class="emph">?</code>,
  <code class="emph">"</code>,
  <code class="emph">p</code>,
  <code class="emph">g</code>,
  <code class="emph">&amp;</code>, and
  <code class="emph">~</code>
  cannot be used.</p>
  
<?php
if (isset($_POST['submit'])) {
  $program = $_POST['program'];
  $data = trim($program, "\r\n") . "\r\n";

  $ch = curl_init();
  curl_setopt($ch, CURLOPT_URL, 'http://localhost:5000/befunge93-api/?restrict');
  curl_setopt($ch, CURLOPT_POST, true);
  curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
  curl_setopt($ch, CURLOPT_HTTPHEADER, array("Content-Type: text/plain")); 
  curl_setopt($ch, CURLOPT_POSTFIELDS, $data);

  $output = curl_exec($ch);
  $response = curl_getinfo($ch, CURLINFO_HTTP_CODE);
  curl_close($ch);

  if (trim($output) == $_SESSION['number']) {
    ?>
    <p class="right">Right!  :-)</p>
    <form <?php echo "action=\"$self\""; ?> id="r" name="r" method="post">
      <input type="hidden" id="reset" name="reset" value="reset" />
      <input type="submit" name="again" id="again" value="Play again!" />
    </form>
    <?php
  } else {
    ?>
    <p class="wrong">Wrong!  :-(</p>
    <p>Your program outputs:</p>
    <pre><?php echo $output; ?></pre>
    <form <?php echo "action=\"$self\""; ?> id="r" name="r" method="post">
      <input type="submit" name="again" id="again" value="Try again!" />
    </form>
    <?php
  }
} else {
  ?>
  <p>Enter your program that will print this number!</p>
  <form <?php echo "action=\"$self\""; ?> id="f" name="f" method="post">
    <textarea name="program" id="program" class="wide" rows=10 cols=80></textarea>
    <br />
    <input type="submit" name="submit" id="submit" value="Submit!" />
    <input type="submit" name="reset" id="reset" value="Change number!" />
  </form>
  <?php
}
?>

</body>
</html>