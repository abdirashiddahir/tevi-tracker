// Coming Soon Tracker - small client helpers
// Open a PlugShare (or any) URL in a new tab from a Shiny custom message.
Shiny.addCustomMessageHandler('openUrl', function(url){
  if (url) window.open(url, '_blank', 'noopener');
});
