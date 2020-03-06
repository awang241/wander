<template>
    <div id="app">
        <NavBar/>
        <div id="nav">

        </div>
        <router-view/>
    </div>
</template>

<script>
  import NavBar from './components/NavBar';

  const app = {
      name: 'app',
      components: {
          NavBar
      },
      data: () => {
          return {
              NavBar
          }
      },
  }
  export default app
</script>

<style>
    [v-cloak] { display: none; }
</style>


