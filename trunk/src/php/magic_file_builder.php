<?php

function do_replacement($file_in, $replace_array)
{
	$data = file_get_contents($file_in . '.txt');

	// do tag replacements or whatever you want
	foreach($replace_array as $key => $value)
		$data = str_replace('{'.strtoupper($key) .'}', $value, $data);	
	
	//$data = str_replace('{WORDS}', $words, $data);
	$data = str_replace('{ACL2_SRC_DIR}', ACL2_SRC_DIR, $data);
	$data = str_replace('{ACL2_TEACHPACKS}', ACL2_TEACHPACKS, $data);

	//make a temp file
	$tmpfname = tempnam(sys_get_temp_dir(), 'acl');

	$handle = fopen($tmpfname, "w");
	fwrite($handle, $data);
	fclose($handle);

	return $tmpfname;
}

?>