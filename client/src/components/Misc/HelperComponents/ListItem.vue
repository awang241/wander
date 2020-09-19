<!--Component for a singular item in a list. This item has text associated with it as well as a button to remove it from the list-->
<template>
    <div class="country">
        <div>
        <p>{{listItem}}</p>
        <b-button type="is-danger" size="is-small" @click="deleteListItem">X</b-button>
        </div>
        <div id="options">
            <slot></slot>
        </div>
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
        display: flex;
        justify-content: space-between;
    }
    #options{
        padding-top: 0.5rem;
    }

</style>