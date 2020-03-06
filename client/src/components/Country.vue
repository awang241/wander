<template>
    <div class="country">
        <p>{{country}}</p>
        <button @click="$emit('deleteCountry', country)" class="btn-delete">x</button>
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

    .btn-delete{
        background: #ff0000;
        color: #fff;
        border: none;
        padding: 5px 9px;
        border-radius: 50%;
        cursor: pointer;
        float: right;
    }
</style>