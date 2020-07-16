<template>
    <div class="container">
        <h1 class="title is-5">Edit Your Location </h1>

        <form>

            <div class="select" id="countrySelectMenu">
                <select v-model="DropdownCountry" id="country" v-on:change="setAutoCompleteCountry" required expanded>
                    <option value="" disabled selected> Must select a country</option>
                    <option v-for="country in possibleCountries" :key="country">
                        {{country}}
                    </option>
                </select>
            </div>
            <br>
            <br>

            <input class="input" type="text" placeholder="Enter a city" id="autocompleteCities"/>
            <br>
            <br>
            <input onkeypress="return /[a-z ]/i.test(event.key)" class="input" type="text" placeholder="Enter a state" id="autocompleteStates"/>

        </form>

        <div class="row">
            <br>
            <b-field style="float:left">
                <b-button type="is-danger" @click="clearLocation()">Clear fields</b-button>
            </b-field>
            <b-field style="float:right">
                <b-button type="is-primary" @click="submitLocation()">Save</b-button>
            </b-field>
            <br>
        </div>
        <br/>
    </div>
</template>

<script>

    import toastMixin from "../../mixins/toastMixin";
    import axios from "axios";

    //Important notes about the API
    //The variables autocompleteCity and autocompleteState are here since Google Maps Places API isn't really compatible with Vue
    //Referencing the DOM via document.getElementById since that's the only way that I found which could extract
    //a Google Maps Places API auto-completed value in an input form

    let autocompleteCity;
    let autocompleteState;



    export default {
        name: "EditLocation",
        props: ["profile"],
        mixins: [toastMixin],
        data() {
            return {
                possibleCountries : "",
                possibleCountriesAlpha2Code : "",
                restrictionCountryAlphaCode: "",
                DropdownCountry: "",
                location : {
                    country: "",
                    city: "",
                    state: "",
                }
            }
        },
        methods: {
            initAutoCompleteCities : function() {
                let options = {
                    types: ['(cities)'],
                    componentRestrictions: {'country' : this.restrictionCountryAlphaCode}
                }
                //eslint-disable-next-line no-undef
                autocompleteCity = new google.maps.places.Autocomplete(document.getElementById("autocompleteCities"), options)
                autocompleteCity.setFields(['address_components'])

                autocompleteCity.addListener('place_changed', function() {
                    var city = autocompleteCity.getPlace();
                    document.getElementById("autocompleteCities").value = city.address_components[0].long_name;
                })

            },
            initAutoCompleteStates : function() {
                let options = {
                    types: ['(regions)'],
                    componentRestrictions: {'country' : this.restrictionCountryAlphaCode}
                }
                //eslint-disable-next-line no-undef
                autocompleteState = new google.maps.places.Autocomplete(document.getElementById("autocompleteStates"), options)
                autocompleteState.setFields(['address_components'])
                autocompleteState.addListener('place_changed', function() {
                    let result = autocompleteState.getPlace();
                    let state = result.address_components[0].long_name;
                    for (let addressComponentIndex in result.address_components) {
                        if (result.address_components[addressComponentIndex].types.includes("administrative_area_level_1")) {
                            state = result.address_components[addressComponentIndex].long_name
                        }
                    }
                    document.getElementById("autocompleteStates").value = state
                })

            },
            getAllCountries(){
                axios.get("https://restcountries.eu/rest/v2/all").then(response => {
                    const data = response.data
                    const possibleCountries = []
                    const possibleCountriesAlpha2Code = []
                    for (let country in data){
                        possibleCountries.push(data[country].name)
                        possibleCountriesAlpha2Code.push(data[country].alpha2Code)
                    }
                    this.possibleCountries = possibleCountries;
                    this.possibleCountriesAlpha2Code = possibleCountriesAlpha2Code;
                    if (this.location.country == "") {
                        this.restrictionCountryAlphaCode = 'NZ';
                        this.DropdownCountry = 'New Zealand';
                        this.location.country = 'New Zealand';
                    } else {
                        var countryIndex = this.possibleCountries.indexOf(this.location.country);
                        this.DropdownCountry = this.possibleCountries[countryIndex];
                        this.restrictionCountryAlphaCode = this.possibleCountriesAlpha2Code[countryIndex];
                    }

                    this.initAutoCompleteCities();
                    this.initAutoCompleteStates()

                }).catch(error => console.log(error));
            },
            setAutoCompleteCountry() {
                var country = document.getElementById('country').value;
                this.location.country = country;
                var countryIndex = this.possibleCountries.indexOf(country)
                document.getElementById("autocompleteCities").value = null;
                document.getElementById("autocompleteStates").value = null;
                autocompleteCity.setComponentRestrictions({'country': this.possibleCountriesAlpha2Code[countryIndex]});
                autocompleteState.setComponentRestrictions({'country': this.possibleCountriesAlpha2Code[countryIndex]});
            },
            clearLocation(){
                this.$parent.clearLocation()
                this.successToast("Location removed")
                document.getElementById("autocompleteCities").value = null;
                document.getElementById("autocompleteStates").value = null;
                document.getElementById("country").value = 'New Zealand';
                this.location = {country: "", city: "", state: ""}
            },
            submitLocation(){
                this.location.city = document.getElementById("autocompleteCities").value;
                this.location.state = document.getElementById("autocompleteStates").value;

                console.log(this.location)
                if(this.location.country === ""){
                    this.warningToast("Please enter a country")
                } else if(this.location.city === ""){
                    this.warningToast("Please enter a city")
                }
                else {
                    this.$parent.updateLocation(this.location)
                    this.successToast("Updated location")
                }
            },
            setLocation(){
                if(this.profile.location != null){
                    this.location = this.profile.location;
                    document.getElementById("country").value = this.location.country;
                    document.getElementById("autocompleteCities").value = this.location.city;
                    document.getElementById("autocompleteStates").value = this.location.state;

                }
            }
        },
        mounted() {
            this.getAllCountries();
            this.setLocation();
        }

    }

</script>

<style scoped>
    .container {
        background-color: #F7F8F9;
        margin-top: 0px;
        padding: 0px;
    }

    .button-right {
        align: right;
    }

    #countrySelectMenu, #country {
        width: 100%;
    }





</style>