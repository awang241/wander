<template>
    <div class="country">
        <p>{{country}}</p>
        <b-button type="is-danger" size="is-small" @click="$emit('deleteCountry', country)">X</b-button>
    </div>
</template>

<script>
    export default {
        name: "Country",
        props: ["country"]
    }
</script>

<style scoped>
    .country{
        background: #f4f4f4;
        padding: 10px;
        border-bottom: 1px #ccc dotted;
    }
</style>