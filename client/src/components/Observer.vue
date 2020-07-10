<!--Observer component used by the profile search page for infinite scrolling-->
<!--Basic intersection code based on tutorial from https://github.com/The-SourceCode-->
<template>
    <div class="observer"></div>
</template>

<script>
    export default {
        name: "Observer",
        data(){
            return {
                observer: null
            }
        },
        mounted() {
            this.setupObserver()
        },
        //Sets up an intersection observer which emits an event to the parent component when it intersects page boundaries
        methods: {
            setupObserver(){
                this.observer = new IntersectionObserver(([entry]) =>{
                    if(entry && entry.isIntersecting){
                        this.$emit('intersect')
                    }
                })
                this.observer.observe(this.$el)
            }
        }
    }
</script>

<style scoped>
    /*Used to detect the observer is at the bottom of the page*/
    .observer{
        height: 1px;
    }
</style>