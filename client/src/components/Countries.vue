<template>
    <div class="container">
        <p>Countries</p>
        <div v-for="country in chosenCountries" v-bind:key="country">
            <Country v-bind:country="country" v-on:deleteCountry="$emit('deleteCountry', country)"></Country>
        </div>
    </div>
</template>

<script>
    import Country from "./Country";
    export default {
        name: "Countries",
        components:{
            Country
        },
        props: ["chosenCountries"]
    }
</script>

<style scoped>

</style>