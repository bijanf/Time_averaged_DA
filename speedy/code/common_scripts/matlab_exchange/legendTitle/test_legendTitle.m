% test the legend title.
x = 0:.2:12;
tString = sprintf ( 'Dev by Matpi Ltd\nwww.matpi.com' );
plot(x,besselj(1,x),x,besselj(2,x),x,besselj(3,x));
hLegend = legend('First','Second','Third','Location','EastOutside');
hTitle = legendTitle ( hLegend, tString, 'FontWeight', 'bold' );
% check that changing the location the title moves
pause(1);
set ( hLegend, 'Location', 'South' );
% check that changing the position the title moves
pause(1);
set ( hLegend, 'position', [0.1 0.1 0.3 0.1] );
% check the main figure size changing the title is updated
screenSize = get ( 0, 'screensize' );
hFig = ancestor ( hTitle, 'figure' );
pause(1);
set ( hFig, 'Position', screenSize );

% check deleting the legend the title is also deleted
pause(1);
delete ( hLegend );

