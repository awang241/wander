<template>
    <div id="app">
        <NavBar/>
        <div id="nav">
        </div>
        <router-view/>
    </div>
</template>

<script>
  import NavBar from './components/General/NavBar';
  import ProfileSearch from "./components/Search/ProfileSearch";
  import router from "./router";

  const app = {
      name: 'app',
      components: {
          ProfileSearch,
          NavBar
      },
      data: () => {
          return {
              NavBar
          }
      },
    mounted() {
        router.push()
    }
  };
  export default app
</script>

<style lang="scss">
    [v-cloak] { display: none; }

    // Import Bulma's core
    @import "~bulma/sass/utilities/_all";

    // Set your colors
    $primary:  #1aa3ff;
    $primary-invert: findColorInvert($primary);

    // Setup $colors to use as bulma classes (e.g. 'is-twitter')
    $colors: (
            "white": ($white, $black),
            "black": ($black, $white),
            "light": ($light, $light-invert),
            "dark": ($dark, $dark-invert),
            "primary": ($primary, $primary-invert),
            "info": ($info, $info-invert),
            "success": ($success, $success-invert),
            "warning": ($warning, $warning-invert),
            "danger": ($danger, $danger-invert),
    );

    // Links
    $link: $primary;
    $link-invert: $primary-invert;
    $link-focus-border: $primary;

    // Import Bulma and Buefy styles
    @import "~bulma";
    @import "~buefy/src/scss/buefy";
</style>


