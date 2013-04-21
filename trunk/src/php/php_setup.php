<?php

function run_setup()
{
	$running_local = false;
	if ($_SERVER['REMOTE_ADDR'] == '::1' || $_SERVER['REMOTE_ADDR'] == '127.0.0.1')
		$running_local = true;

	if ($running_local)
	{
		if (isset ($_COOKIE['ACL2_EXE_DIR']) && isset ($_COOKIE['ACL2_SRC_DIR']) && isset ($_COOKIE['ACL2_TEACHPACKS']))
		{
			if (file_exists($_COOKIE['ACL2_EXE_DIR']) && is_dir($_COOKIE['ACL2_SRC_DIR']) && is_dir($_COOKIE['ACL2_TEACHPACKS']))
			{
				//run with our commands
				set_directories($_COOKIE['ACL2_EXE_DIR'], $_COOKIE['ACL2_SRC_DIR'], $_COOKIE['ACL2_TEACHPACKS']);
				return true;
			}
			else
			{
				echo '<p>Problem with directories, try again.</p>';
				unset_directories();
			}
		}
		elseif (isset ($_POST['ACL2_EXE_DIR']) && isset ($_POST['ACL2_SRC_DIR']) && isset ($_POST['ACL2_TEACHPACKS']))
		{
			if (file_exists($_POST['ACL2_EXE_DIR']) && is_dir($_POST['ACL2_SRC_DIR']) && is_dir($_POST['ACL2_TEACHPACKS']))
			{
				//run with our commands
				set_directories($_POST['ACL2_EXE_DIR'], $_POST['ACL2_SRC_DIR'], $_POST['ACL2_TEACHPACKS']);
				return true;
			}
			else
			{
				echo '<p>Problem with directories, try again.</p>';
				unset_directories();
			}
		}

		//print a form to enter stuffs
		echo '<form method="post" action="">';
		echo '<span class="label">ACL2 EXE Directory</span><input type="text" name="ACL2_EXE_DIR" /><br />';
		echo '<span class="label">ACL2 SRC Directory</span><input type="text" name="ACL2_SRC_DIR" /><br />';
		echo '<span class="label">ACL2 Teachpacks</span><input type="text" name="ACL2_TEACHPACKS" /><br />';
		echo '<input type="submit" value="Submit"></form>';
	}
	else
	{
		// not running local, use preset directories
		$ACL2_EXE_DIR = 'C:/ACL2/run_acl2.exe';
		$ACL2_SRC_DIR = 'C:/Users/Jakob/Desktop/School/Spring2013/se/svn/src/';
		$ACL2_TEACHPACKS = 'C:/Users/Jakob/AppData/Roaming/Racket/planet/300/5.2.1/cache/cce/dracula.plt/8/23/teachpacks';

		set_directories($ACL2_EXE_DIR, $ACL2_SRC_DIR, $ACL2_TEACHPACKS);
		return true;
	}

	return false;
}

function set_directories($ACL2_EXE_DIR, $ACL2_SRC_DIR, $ACL2_TEACHPACKS)
{
	define('ACL2_EXE_DIR', $ACL2_EXE_DIR);
	define('ACL2_SRC_DIR', $ACL2_SRC_DIR);
	define('ACL2_TEACHPACKS', $ACL2_TEACHPACKS);

	set_cookies(time() + 36000);
}

function unset_directories()
{
	set_cookies(time() - 36000);
}

function set_cookies($time)
{
	setcookie('ACL2_EXE_DIR', ACL2_EXE_DIR, $time);
	setcookie('ACL2_SRC_DIR', ACL2_SRC_DIR, $time);
	setcookie('ACL2_TEACHPACKS', ACL2_TEACHPACKS, $time);
}

// thanks to http://stackoverflow.com/questions/834303/php-startswith-and-endswith-functions
function starts_with($haystack, $needle)
{
    return !strncmp($haystack, $needle, strlen($needle));
}

function ends_with($haystack, $needle)
{
    $length = strlen($needle);
    if ($length == 0)
        return true;

    return (substr($haystack, -$length) === $needle);
}

?>