<?php
include 'magic_file_builder.php';

$action = (isset ($_REQUEST['action']) ? $_REQUEST['action'] : '');

switch ($action)
{
	#============================================================
	# Default
	#============================================================
	default :
		$json = array (
			'success' => false
		);
		//code here
		//filter_input(INPUT_GET, 'the_table', FILTER_SANITIZE_STRING);
		echo json_encode($json);
		exit (0);
		break;

	case 'check_letter' :
		$json = array (
			'success' => false
		);
		
		if (isset ($_COOKIE['ACL2_EXE_DIR']) && isset ($_COOKIE['ACL2_SRC_DIR']) && isset ($_COOKIE['ACL2_TEACHPACKS']))
		{
			if (file_exists($_COOKIE['ACL2_EXE_DIR']) && is_dir($_COOKIE['ACL2_SRC_DIR']) && is_dir($_COOKIE['ACL2_TEACHPACKS']))
			{
				// set our directories				
				define('ACL2_EXE_DIR', $_COOKIE['ACL2_EXE_DIR']);
				define('ACL2_SRC_DIR', $_COOKIE['ACL2_SRC_DIR']);
				define('ACL2_TEACHPACKS', $_COOKIE['ACL2_TEACHPACKS']);
		
				define('SETUP_TEMPLATE', 'setup/check_board_template');
		
				$x = filter_input(INPUT_GET, 'x', FILTER_SANITIZE_NUMBER_INT);
				$y = filter_input(INPUT_GET, 'y', FILTER_SANITIZE_NUMBER_INT);
				$letter = filter_input(INPUT_GET, 'letter', FILTER_SANITIZE_STRING);
				$solution = urldecode(filter_input(INPUT_GET, 'solution', FILTER_SANITIZE_STRING));
				$solution = str_replace('&#34;', '"', $solution);
				$solution = preg_replace('/\s+/', ' ', $solution);
				$solution = trim($solution);
				
				// then make our setup file
				$SETUP = do_replacement(ACL2_SRC_DIR . SETUP_TEMPLATE, array('X' => $x,
																			 'Y' => $y,
																			 'LETTER' => $letter,
																			 'SOLUTIONS' => $solution));
																			 
				$final_call = ACL2_EXE_DIR . ' < ' . $SETUP;
				exec($final_call, $console_log);
				
				//echo $final_call;
				
				$returned_string = $console_log[count($console_log) - 2];
				if($returned_string == 'ACL2 p>#\\' . $letter)
				{
					$json['success'] = true;
					$json['correct'] = true;
				}
				else if($returned_string == 'ACL2 p>NIL')
				{
					$json['success'] = true;
					$json['correct'] = false;	
				}
																	 
				//var_dump ($console_log);
				
				unlink($SETUP);
			}
		}
		
		echo json_encode($json);
		exit (0);
		break;
}
?>