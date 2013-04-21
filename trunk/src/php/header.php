<?php

function header_print()
{

echo '<!DOCTYPE html><html><head>' .
'<!-- no cache -->' .
'<meta http-equiv="pragma" content="no-cache" />' .
'<meta http-equiv="expires" content="-1" />' .

'<!-- jquery -->' .
'<script src="js/jquery.js" type="text/javascript"></script>' .
'<script src="js/javascript.js" type="text/javascript"></script>' .

'<!-- style -->' .
'<LINK href="css/style.css" rel="stylesheet" type="text/css">' .

'<!-- everything else -->' .
'<title>ACL2 Project</title>' .
'</head><body>';

echo '<center><h1>ACL2 WordGames</h1></center>';
echo '<center><h3><a href="make_play_search.php">WordSearch</a> &middot; CrossWord &middot; <a href="make_play_solver.php">Solver</a></h3></center>';

echo '<div class="center">';

echo '<div class="pad10">';
}

?>