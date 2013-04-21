<?php

include 'header.php';

header_print();

echo '<div class="pad10">';

echo 'Welcome to the <span class="red">Team VanRossum</span> series of WordGames. Programmed in ACL2 (with some magic php sprinkled in) we are able to bring you' .
		'a whole host of fun including a WordSearch generator, CrossWord puzzle generator, and a Solver for both.';

echo '<br /><br />';

echo 'The individuals who make up Team VanRossume include CS students in Dr. Page\'s Software Engineering II class at the University of Oklahoma.';
echo '<br />';
echo '<center><span class="green">Cezar Delucca</span>&middot;<span class="green">Olufemi Fashanu</span>' .
		'&middot;<span class="green">Jakob Griffith</span>&middot;<span class="green">Clayton Miller</span>' .
		'&middot;<span class="green">Shane Moore</span></center>';

echo '</div>';

include 'tailer.php';

?>