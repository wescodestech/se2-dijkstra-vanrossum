<?php
include 'header.php';
require_once 'php_setup.php';
require_once 'magic_file_builder.php';

if (run_setup())
{
	header_print();
	run_create_board();
}

function run_create_board()
{
	define('SETUP_TEMPLATE', 'setup/create_board_template');

	if (isset ($_GET['words']))
	{
		echo 'Click letters to play the wordsearch or <a href="make_play_search.php">click here</a> to make a new board.<br /><br />';

		// clean things up
		$words = preg_split("/[\s,~.\-_\n]+/", $_GET['words']);
		foreach ($words as $key => $word)
		{
			$temp_word = trim($word);
			if (!empty ($temp_word))
				$words[$key] = $temp_word;
			else
				unset ($words[$key]);
		}
		$implode_words = '"' . implode('" "', $words) .'"';

		// then make our setup file
		$SETUP = do_replacement(ACL2_SRC_DIR . SETUP_TEMPLATE, array('list_words' => $implode_words));

		echo '<table id="main_table"><tr><td>';

		//output the words
		echo '<ul id="word_list">';
		foreach ($words as $word)
			echo '<li>' . $word . '</li>';
		echo '</ul>';

		echo '</td><td><center>';

		// run command
		// NOTE for now this uses redirected input, but it will eventually
		// use clay's ACL2 code
		$final_call = ACL2_EXE_DIR . ' < ' . $SETUP;
		exec($final_call, $console_log);

		//echo '<p>Command: ' . $final_call . '</p>';

		$output_found = false;
		foreach ($console_log as $key => $results)
		{
			$head = 'ACL2 p>("[[';
			$tail = ']]"';

			if(starts_with($results, $head) &&
				stristr($results, $tail))
				{
					// first the board output
					$explode_results = explode($tail, $results);
					$results = $explode_results[0] . $tail;

					echo '<p id="board_json">';
					$json = '';

					//then just clean it
					$json = str_replace($head, '[["', $results);
					$json = str_replace($tail, '"]]', $json);

					//first add back in our quotes
					$json = str_replace('],[', ';', $json);
					$json = str_replace(',', '","', $json);
					$json = str_replace(';', '"],["', $json);

					echo $json;

					echo '</p>';

					$output_found = true;

					echo '<p id="board_solution">';
				}
				else if($output_found)
				{
					if(starts_with($results, 'ACL2 p>Bye.'))
					{
						echo '</p>';
						break;
					}

					echo str_replace('))))', ')))', $results);
				}

		}

		echo '</center></td></tr></table>';

		echo '<div id="output_display">Show Output</div>';
		echo '<div id="acl2_output">';
		var_dump ($console_log);
		echo '</div>';

		//delete setup
		unlink($SETUP);
	}
	else
	{
		echo 'Enter a list of words to make a wordsearch. The words can be seperated by space, dash, comma, underscore, or newlines.<br /><br />';

		echo '<form method="get" action=""><center><textarea name="words">' . (isset ($_GET['words']) ? $_GET['words'] : 'Put your words here') . '</textarea>' .
			'<br /><br /><input type="submit" value="Submit"></center></form>';
	}
}

include 'tailer.php';