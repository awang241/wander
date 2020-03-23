<!--Container component that Holds several ListItems-->
<template>
    <div>
        <div v-for="listItem in chosenItems" v-bind:key="listItem">
            <ListItem v-bind:listItem="listItem" v-on:deleteListItem="$emit('deleteListItem', listItem)"></ListItem>
        </div>
    </div>
</template>

<script>
    import ListItem from "./ListItem";
    export default {
        name: "List",
        components:{
            ListItem
        },
        props: ["chosenItems"]
    }
</script>
