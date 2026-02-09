const main = () =>
  (() => {
    async(() =>
      (() => {
        await(foo(), (data) => pure(data.name));
      })(),
    );
  })();
