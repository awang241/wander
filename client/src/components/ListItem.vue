<!--Component for a singular item in a list. This item has text associated with it as well as a button to remove it from the list-->
<template>
    <div class="country">
        <p>{{listItem}}</p>
        <b-button type="is-danger" size="is-small" @click="deleteListItem">X</b-button>
    </div>
</template>

<script>
    export default {
        name: "ListItem",
        props: ["listItem"],
        methods: {
            deleteListItem(){
                this.$emit('deleteListItem', this.listItem)
            }
        }
    }
</script>

<style scoped>
    .country{
        background: #f4f4f4;
        padding: 10px;
        border-bottom: 1px #ccc dotted;
    }
</style>