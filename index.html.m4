<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width">
    <title>Delia's Game</title>
    <script>
//<![CDATA[
undivert(`mini.js')
//]]>

    </script>
</head>
<body>
<script>
  var randomSeed = new Uint32Array(4);
  window.crypto.getRandomValues(randomSeed);
  var app = Elm.Main.init({
  flags: {
    'hasMouse': window.matchMedia('(pointer:fine)').matches,
    'randomSeed': Array.from(randomSeed)
  }
  });

</script>
</body>
</html>
