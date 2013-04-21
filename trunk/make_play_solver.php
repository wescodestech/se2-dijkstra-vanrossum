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
	define('SETUP_TEMPLATE', 'setup/solve_board_template');

	if (isset ($_POST['submit']))
	{
		echo 'Check out the solved board or <a href="make_play_solver.php">click here</a> to solve a new board.<br /><br />';

		// clean things up
		$words = preg_split("/[\s,~.\-_\n]+/", $_POST['words']);
		foreach ($words as $key => $word)
		{
			$temp_word = trim($word);
			if (!empty ($temp_word))
				$words[$key] = $temp_word;
			else
				unset ($words[$key]);
		}
		$clean_words = implode("\n", $words);
		$implode_words = '"' . implode('" "', $words) .'"';

		$weird_words = split_words($clean_words);

		$board = $_POST['board'];
		$board_final = split_words($board);

		$type = $_POST['solve_type'];

		// then make our setup file
		$SETUP = do_replacement(ACL2_SRC_DIR . SETUP_TEMPLATE, array('list_words' => $weird_words,
																	 'BOARD' => $board_final,
																	 'SOLVER_TYPE' => $type));

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

		// and build a board which we can actually solve
		echo '<p id="board_json">';

		//must be json
		$board_json = str_replace('(', '[', $board_final);
		$board_json = str_replace(')', ']', $board_json);
		$board_json = str_replace('" "', '", "', $board_json);
		$board_json = str_replace('] [', '], [', $board_json);

		echo '[' . $board_json . ']';

		echo '</p>';

		foreach ($console_log as $key => $results)
		{
			$head = 'ACL2 p>"[{';
			$tail = '}]"';

			if(starts_with($results, $head) &&
				stristr($results, $tail))
				{
					// first the board output
					$explode_results = explode($tail, $results);
					$results = $explode_results[0] . $tail;

					echo '<p id="solved_json">';
					$json = '';

					//then just clean it
					$json = str_replace($head, '[{"', $results);
					$json = str_replace($tail, '"}]', $json);

					//first add back in our quotes
					$json = str_replace('],[', ';', $json);
					$json = str_replace(',', '","', $json);
					$json = str_replace(';', '"],["', $json);
					$json = str_replace('}","{', '"}, {"', $json);
					$json = str_replace(':', '":"', $json);

					echo $json;

					echo '</p>';
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
		echo 'Enter a generated board and your prefered solving method to have the wordsearch solved.<br /><br />';

		echo '<form method="post" action=""><center><table id="main_table"><tr><td>' .
				'<center><textarea name="words" class="solver_textarea">Put your example list of words here, and your board over there</textarea></center>' .
				'</td><td>' .
				'<center><textarea name="board" class="solver_textarea">' .
				'example&#10;' . // 1
				'wouldlo&#10;' . // 2
				'likethi&#10;' . // 3
				'isboard&#10;' . // 4
				'righthe&#10;' . // 5
				'looksli&#10;' . // 6
				'theirgh&#10;' . // 7
				'</textarea></center>' .
				'</td></tr></table>' .
			'<br /> <select name="solve_type">' .
			'<option value="1">Brute Force</option>' .
			'<option value="2">Hill Climber</option>' .
			'</select><br /><br />' .
			'<input name="submit" type="submit" value="Submit"></center></form>';
	}
}

function split_words($board)
{
	$board = str_replace("\n\r", "\n", $board);
		$board = str_replace("&#10;", "\n", $board);
		$board_explode = explode("\n", $board);
		foreach($board_explode as $key => $row)
		{
			$split = str_split(trim($row));
			$treated = '"' . implode('" "', $split) . '"';
			if($treated != '""')
				$board_explode[$key] = '(' . $treated . ')';
		}
		$board_final = implode(' ', $board_explode);
		return $board_final;
}

include 'tailer.php';