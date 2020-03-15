<template>
    <section>
        <b-field>
            <b-select placeholder="Select a country" v-model="country">
                <option
                        v-for="country in possibleCountries"
                        :value="country"
                        :key="country">
                    {{ country }}
                </option>
            </b-select>
            <b-button type="is-primary" @click="addCountry">Submit</b-button>
        </b-field>
    </section>
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