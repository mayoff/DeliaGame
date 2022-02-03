<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width">
    <title>Delia's Game</title>
    <script>
//<![CDATA[
undivert(`main.js')
//]]>
</script>
</head>
<body>
<div id="appnode"></div>
<script>
  var randomSeed = new Uint32Array(4);
  window.crypto.getRandomValues(randomSeed);
  var app = Elm.Main.init({
  node: document.getElementById('appnode'),
  flags: {
    'hasMouse': window.matchMedia('(pointer:fine)').matches,
    'randomSeed': Array.from(randomSeed)
  }
  });
</script>
</body>
</html>
