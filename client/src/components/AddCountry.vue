<template>
    <form @submit="addCountry">
<!--        <input type="text" v-model="country" name="country" placeholder="Add country">-->
        <select v-model="country">
            <option v-for="country in possibleCountries" v-bind:key="country">{{country}}</option>
        </select>
        <input type="submit" value="submit">
    </form>
</template>

<script>
    export default {
        name: "AddCountry",
        props: ["possibleCountries", "chosenCountries"],
        data() {
            return{
                country: ""
            }
        },
        methods: {
            addCountry(e) {
                e.preventDefault()
                const newCountry = {
                    name: this.country,
                }
                if(!this.chosenCountries.includes(newCountry)){
                    this.$emit('addCountry', newCountry)
                }
            }
        }
    }
</script>

<style scoped>

</style>