var green = '#33FF33';
var red = 'red';

$(document).ready(function(){
	
	/* convert json to board */
	var json = $('#board_json').html();
	
	if(json != undefined && json != '')
	{
		var obj = jQuery.parseJSON(json);
	
		var table = '<div>'
		$.each(obj, function(i, item) {
	   
			table += '<div>'
			$.each(item, function(e, row) {
		 
				table += '<span x="'+ i +'" y="'+ e +'" class="border table_letter">' + row + '</span>';
			
			});
			table += '<div>'
		
		});
		table += '</div>'
		
		$('#board_json').html(table);
	}
	
	/* show or hide output */
	
	$('#output_display').on('click', function(){
		$('#acl2_output').slideToggle();
	});
	
	/* and solve the board */
	
	$('.table_letter').on('click', function(){
		var my_this = $(this);
		
		var x = $(this).attr('x');
		var y = $(this).attr('y');
		var letter = $(this).html();
		
		var solution = $('#board_solution').html();
		
		$.getJSON('wordsearch_play.php', {'action': 'check_letter',
			'letter': letter,
			'x': x,
			'y': y,
			'solution': solution}, function(j) {
			if(j.success)
			{
				if(j.correct)
					my_this.css('background-color', green);
				else
					my_this.css('background-color', red);
			}
		});	
	});
	
	/* solver json */
	
	var solver_json = $('#solved_json').html();
	if(solver_json != undefined && solver_json != '')
	{
		var obj = jQuery.parseJSON(solver_json);
	
		$.each(obj, function(i, item){
			var x = parseInt(item.x);
			var y = parseInt(item.y);
			var direction = item.direction;
			var spaces = parseInt(item.numberofspaces) + 1;
			
			if(spaces != 0)
			{
				if(direction == 'up')
				{
					for (var i = 0; i < spaces; i++)
					{ 
						set_green(x - i, y);
					}
				}
				else if(direction == 'down')
					for (var i = 0; i < spaces; i++)
						set_green(x + i, y);
				else if(direction == 'left')
					for (var i = 0; i < spaces; i++)
						set_green(x, y - i);
				else if(direction == 'right')
					for (var i = 0; i < spaces; i++)
						set_green(x, y + i);
			}
		});
		
		$('#solved_json').html('');
	}
});

function set_green(x, y)
{
	var letters = $('.table_letter');
	$.each(letters, function(i, item){
		var tx = $(this).attr('x');
		var ty = $(this).attr('y');
		
		var my_this = $(this);
		
		if(tx == x && ty == y)
		{
			my_this.css('background-color', green);
			return false;
		}
	});
}