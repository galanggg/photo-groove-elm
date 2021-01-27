let app = Elm.PhotoGroove.init({
  node: document.getElementById('app'),
  flags: Pasta.version,
});

app.ports.setFilters.subscribe(function (options) {
  requestAnimationFrame(function () {
    Pasta.apply(document.getElementById('main-canvas'), options);
  });
});

Pasta.addActivityListener(function (activity) {
  console.log('Got some message to send to Elm', activity);
  app.ports.activityChanges.send(activity);
});
