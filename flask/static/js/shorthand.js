/**
 * Created by chris on 15/07/15.
 */

$(document).ready(function() {

        $('.toggle_button').click(function(){
            var text = $(this).text();

            if (text=="-"){
                $(this).text('+').button("refresh");
                $(this).next().slideUp(250);
            }
            if (text=="+"){
                $(this).text('-').button("refresh");
                $(this).next().slideDown(250);
            }

        });


});